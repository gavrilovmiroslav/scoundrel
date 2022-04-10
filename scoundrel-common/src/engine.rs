use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;
use std::sync::{Arc, Mutex, RwLock};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc::{channel, Receiver, TryRecvError};
use std::time::Duration;
use std::time::Instant;

use lazy_static::*;
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher, watcher};

use lazy_static;

use crate::engine::ChangeEvent::Write;
use crate::engine_options::EngineOptions;
use crate::glyphs::Glyph;
use crate::keycodes::{ElementState, KeyState, MouseState};
use crate::point::Point;
use crate::presentation::Presentation;
use crate::rascal::parser::{ComponentType, parse_rascal, RascalStruct, SystemPrioritySize};
use crate::rascal::world::{CACHED_SYSTEMS_BY_PRIORITIES, REGISTERED_SYSTEMS, SYSTEM_DEPENDENCIES};
use crate::rascal::world::World;

#[derive(Clone, Default)]
pub struct FrameCounter {
    active_frame_count: Arc<AtomicU64>,
    last_cached_count: Arc<AtomicU64>,
}

impl FrameCounter {
    pub fn tick(&mut self) {
        self.active_frame_count.fetch_add(1, Ordering::Acquire);
    }

    pub fn cache(&mut self) {
        self.last_cached_count.store(self.active_frame_count.fetch_min(0, Ordering::Relaxed), Ordering::Release);
    }

    pub fn cached(&self) -> u64 {
        self.last_cached_count.load(Ordering::Acquire)
    }
}

#[derive(Clone)]
pub struct Screen {
    pub screen_memory: Option<Vec<Glyph>>,
    pub symbol_depth: Vec<SystemPrioritySize>,
    pub fg_depth: Vec<SystemPrioritySize>,
    pub bg_depth: Vec<SystemPrioritySize>,
    pub size: (u32, u32),
    pub limit: usize,
}

impl Screen {
    pub fn new() -> Screen {
        Screen {
            screen_memory: None,
            symbol_depth: Vec::new(),
            fg_depth: Vec::new(),
            bg_depth: Vec::new(),
            size: (0, 0),
            limit: 0,
        }
    }

    pub fn is_ready(&self) -> bool {
        self.screen_memory.is_some()
    }

    pub fn glyphs_mut(&mut self) -> &mut Vec<Glyph>{
        self.screen_memory.as_mut().unwrap()
    }

    pub fn glyphs(&self) -> &Vec<Glyph>{
        self.screen_memory.as_ref().unwrap()
    }

    pub fn set_memory(&mut self, size: (u32, u32), memory: Vec<Glyph>) {
        self.size = size;
        self.limit = (size.0 * size.1) as usize;

        self.symbol_depth = Vec::with_capacity(self.limit);
        for i in 0..self.limit { self.symbol_depth.push(0); }

        self.fg_depth = Vec::with_capacity(self.limit);
        for i in 0..self.limit { self.fg_depth.push(0); }

        self.bg_depth = Vec::with_capacity(self.limit);
        for i in 0..self.limit { self.bg_depth.push(0); }

        self.screen_memory = Some(memory);
    }

    pub fn reinit_depth(&mut self) {
        for e in &mut self.symbol_depth { *e = 0; }
        for e in &mut self.fg_depth { *e = 0; }
        for e in &mut self.bg_depth { *e = 0; }
    }

    pub fn get_memory(&self) -> *const Glyph {
        self.screen_memory.as_ref().unwrap().as_ptr()
    }

    pub fn len(&self) -> usize {
        self.screen_memory.as_ref().map(|v| v.len()).unwrap_or(0)
    }

    pub fn get_index_for_position(&self, position: (u32, u32)) -> u32 {
        position.1 * self.size.0 + position.0
    }
}

lazy_static! {
    pub static ref ENGINE_OPTIONS: Arc<Mutex<EngineOptions>> = Arc::new(Mutex::new(EngineOptions::default()));
    pub static ref PRESENTATIONS: Arc<Mutex<HashMap<String, Presentation>>> = Arc::new(Mutex::new(HashMap::new()));
    pub static ref SHOULD_QUIT: Arc<AtomicBool> = Arc::new(AtomicBool::new(false));
    pub static ref STOPWATCH: Arc<Mutex<Instant>> = Arc::new(Mutex::new(Instant::now()));
    pub static ref FRAME_COUNTER: Arc<Mutex<FrameCounter>> = Arc::new(Mutex::new(FrameCounter::default()));
    pub static ref KEYBOARD_EVENTS: Arc<Mutex<VecDeque<(KeyState, ElementState)>>> = Arc::new(Mutex::new(VecDeque::default()));
    pub static ref MOUSE_EVENTS: Arc<Mutex<VecDeque<MouseState>>> = Arc::new(Mutex::new(VecDeque::default()));
    pub static ref MOUSE_POSITIONS: Arc<Mutex<Point>> = Arc::new(Mutex::new((0, 0).into()));
    pub static ref SCREEN: Arc<RwLock<Screen>> = Arc::new(RwLock::new(Screen::new()));
    pub static ref SHOULD_REDRAW: Arc<AtomicBool> = Arc::new(AtomicBool::new(true));
    pub static ref WORLD: Arc<Mutex<World>> = Arc::new(Mutex::new(World::default()));
    pub static ref WATCHER: Mutex<Option<RecommendedWatcher>> = Mutex::new(None);
    pub static ref WATCH_RECEIVER: Mutex<Option<Receiver<DebouncedEvent>>> = Mutex::new(None);
}

#[derive(PartialEq, Debug)]
pub enum ChangeEventError {
    Generic(String),
    IO,
    PathNotFound,
    WatchNotFound,
}

#[derive(PartialEq, Debug)]
pub enum ChangeEvent {
    NoticeWrite(PathBuf),
    NoticeRemove(PathBuf),
    Create(PathBuf),
    Write(PathBuf),
    Chmod(PathBuf),
    Remove(PathBuf),
    Rename(PathBuf, PathBuf),
    Rescan,
}

#[derive(PartialEq, Debug)]
pub enum DataChange {
    Change(ChangeEvent),
    NoChange,
    Disconnected,
    Error(ChangeEventError, Option<PathBuf>),
}

pub fn get_filename_when_changed() -> Option<PathBuf> {
    use DataChange::Change;
    use ChangeEvent::Write;

    if let Some(Change(Write(p))) = snoop_for_data_changes() {
        Some(p)
    } else {
        None
    }
}

pub fn snoop_for_data_changes() -> Option<DataChange> {
    use ChangeEvent::*;
    use ChangeEventError::*;
    use DataChange::*;
    use DebouncedEvent as E;

    match WATCH_RECEIVER.lock().unwrap().as_ref().unwrap().try_recv() {
        Ok(E::NoticeWrite(p)) => Some(Change(NoticeWrite(p))),
        Ok(E::NoticeRemove(p)) => Some(Change(NoticeRemove(p))),
        Ok(E::Create(p)) => Some(Change(Create(p))),
        Ok(E::Write(p)) => Some(Change(Write(p))),
        Ok(E::Chmod(p)) => Some(Change(Chmod(p))),
        Ok(E::Remove(p)) => Some(Change(Remove(p))),
        Ok(E::Rename(p, q)) => Some(Change(Rename(p, q))),
        Ok(E::Rescan) => Some(Change(Rescan)),
        Ok(E::Error(notify::Error::Generic(s), op)) => Some(Error(Generic(s), op)),
        Ok(E::Error(notify::Error::Io(_), op)) => Some(Error(IO, op)),
        Ok(E::Error(notify::Error::WatchNotFound, op)) => Some(Error(WatchNotFound, op)),
        Ok(E::Error(notify::Error::PathNotFound, op)) => Some(Error(PathNotFound, op)),
        Err(TryRecvError::Disconnected) => Some(Disconnected),
        Err(TryRecvError::Empty) => None,
    }
}

pub fn start_engine(opts: EngineOptions) {
    *ENGINE_OPTIONS.lock().unwrap() = opts;

    let mut presentations = PRESENTATIONS.lock().unwrap();

    for maybe_file in std::fs::read_dir("resources/data/presentations/").unwrap() {
        if let Ok(file) = maybe_file {
            if let Ok(ron) = std::fs::read_to_string(file.path()) {
                let maybe_config: Result<Presentation, _> = ron::from_str(ron.as_str());
                if let Ok(config) = maybe_config {
                    presentations.insert(file.path().file_stem().unwrap().to_str().unwrap().to_string(), config);
                    println!("Added {:?} to presentations!", file.file_name());
                }
            }
        }
    }

    if presentations.is_empty() {
        panic!("No presentations, quitting!");
    }

    if let Ok(mut file_watcher) = WATCHER.lock() {
        let (tx, rx) = channel();
        *file_watcher = watcher(tx, Duration::from_secs(1)).ok();

        if let Ok(_) = file_watcher.as_mut().unwrap().watch("resources/data", RecursiveMode::Recursive) {
            *WATCH_RECEIVER.lock().unwrap() = Some(rx);
        } else {
            panic!("WATCHER CAN'T WATCH THIS FOLDER!");
        }
    }

    {
        let mut world = WORLD.lock().unwrap();
        world.register_component(RascalStruct::Tag("Main".to_string()));
    }

    rebuild_world("prelude.rascal");
}

pub fn set_should_redraw() { SHOULD_REDRAW.store(true, Ordering::Release) }

pub fn reset_should_redraw() { SHOULD_REDRAW.store(false, Ordering::Release) }

pub fn should_redraw() -> bool { SHOULD_REDRAW.load(Ordering::Acquire) }

pub fn should_quit() -> bool {
    SHOULD_QUIT.load(Ordering::Acquire)
}

pub fn force_quit() {
    SHOULD_QUIT.store(true, Ordering::Release);
}

fn remove_structure(world: &mut World, structure: &RascalStruct) {
    match structure {
        RascalStruct::State(state) => {
            world.registered_states.remove(&state.name);
            world.storage_pointers.remove(&state.name);
            world.storage_bitmaps.remove(&state.name);
        }
        RascalStruct::Event(event) => {
            world.registered_events.remove(&event.name);
            world.event_queues.remove(&event.name);
            world.next_event_queues.remove(&event.name);
        }
        RascalStruct::Tag(tag) => {
            world.registered_tags.remove(tag);
            world.storage_bitmaps.remove(tag);
        }
        RascalStruct::System(sys) => {
            REGISTERED_SYSTEMS.lock().unwrap().remove(&sys.name);
            SYSTEM_DEPENDENCIES.lock().unwrap().remove(&sys.name);
            CACHED_SYSTEMS_BY_PRIORITIES.lock().unwrap().remove(&sys.name);
        }
        RascalStruct::Proc(name, members, block) => {

        }
        RascalStruct::Unique(name, datatype, subtype, val) => {
            unreachable!()
        }
    }
}

pub fn rebuild_world(source_name: &str) {
    println!("[========================================= REBUILDING WORLD ({}) =========================================================]", source_name);

    let mut world = WORLD.lock().unwrap();

    if let Ok(data) = world.data.get(source_name) {
        let src = std::str::from_utf8(data.as_slice());
        if let ast = parse_rascal(src.unwrap()) {
            use crate::rascal::world::SYSTEM_DEPENDENCIES;
            use crate::rascal::world::CACHED_SYSTEMS_BY_PRIORITIES;

            world.clear_entities();

            if world.definitions_in_source.contains_key(source_name) {
                for (id, typ) in world.definitions_in_source.get(source_name).unwrap().clone() {
                    println!("[!] Resetting {}", id);
                    match typ {
                        ComponentType::State => {
                            world.component_types.remove(&id);
                            world.registered_states.remove(&id);
                            world.storage_bitmaps.remove(&id);
                            world.storage_pointers.remove(&id);
                        }
                        ComponentType::Event => {
                            world.component_types.remove(&id);
                            world.registered_events.remove(&id);
                            world.event_queues.remove(&id);
                            world.next_event_queues.remove(&id);
                        }
                        ComponentType::Tag => {
                            world.component_types.remove(&id);
                            world.registered_tags.remove(&id);
                            world.storage_bitmaps.remove(&id);
                        }
                        ComponentType::System => {
                            REGISTERED_SYSTEMS.lock().unwrap().remove(&id);
                        }
                        ComponentType::Unique => {
                            println!("THIS UNREACHABLE NEEDS TO BE FIXED!");
                            unreachable!()
                        }
                        ComponentType::Proc => {
                            println!("THIS UNREACHABLE NEEDS TO BE FIXED!");
                            unreachable!()
                        }
                    }
                }

                world.definitions_in_source.remove(source_name);
            }

            world.definitions_in_source.insert(source_name.to_string(), Vec::new());
            for structure in ast {
                println!("Defining {}", structure.get_name());
                world.definitions_in_source.get_mut(source_name).unwrap().push((structure.get_name().to_string(), structure.get_type()));

                if structure.is_component() {
                    world.register_component(structure);
                } else {
                    world.register_system(structure);
                }
            }

            let deps = SYSTEM_DEPENDENCIES.lock().unwrap();
            let mut cache = CACHED_SYSTEMS_BY_PRIORITIES.lock().unwrap();
            for comp in deps.keys() {
                println!("CACHED EVENT TRACE for {}: \n\t{:?}", comp, deps.get(comp).unwrap().clone().into_sorted_iter().map(|(s, p)| s).collect::<Vec<_>>());
                cache.insert(comp.clone(), deps.get(comp).unwrap().clone().into_sorted_iter().map(|(s, p)| s).collect());
            }
        }
    } else {
        println!("Error: script file {} not found.", source_name);
    }
    println!("[=========================================================================================================================]");
}

pub fn update_world() {
    WORLD.lock().unwrap().run_all_systems();
}