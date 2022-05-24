use crate::core::glyphs::NativeGlyph;
use crate::core::input::{Input, InputState};
use crate::core::point::Point;
use crate::core::presentation::Presentation;
use crate::graphics::glyph_renderer::GlyphRenderer;
use crate::rand;
use gilrs::Gilrs;
use glutin::{EventsLoop, WindowedContext};
use lazy_static::lazy_static;
use notify::DebouncedEvent;
use notify::RecommendedWatcher;
use notify::{watcher, RecursiveMode, Watcher};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
#[allow(unused_imports)]
use std::collections::HashSet;
use std::{fs};
use std::path::Path;
use std::path::PathBuf;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::{channel, TryRecvError};
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

lazy_static! {
    pub static ref ENGINE_STATE: Arc<Mutex<EngineState>> =
        Arc::new(Mutex::new(EngineState::default()));
}

#[derive(Default)]
pub struct EngineRuntimeState {
    pub should_quit: bool,
    pub options: EngineOptions,
    pub presentations: HashMap<String, Presentation>,
}

#[derive(Default)]
pub struct EngineInputState {
    pub input_events: HashMap<Input, InputState>,
    pub mouse_position: Point,
}

#[derive(Default)]
pub struct EngineRenderState {
    pub screen: Screen,
    pub should_redraw: bool,
    pub screen_size: (u32, u32),
}

#[derive(Default)]
pub struct EngineLiveSyncState {
    pub file_watcher: Option<RecommendedWatcher>,
    pub watch_receiver: Option<Receiver<DebouncedEvent>>,
}

#[derive(Default)]
pub struct EngineState {
    pub runtime_state: EngineRuntimeState,
    pub input_state: EngineInputState,
    pub render_state: EngineRenderState,
    pub live_sync_state: EngineLiveSyncState,
}

#[derive(Clone, Default)]
pub struct Screen {
    pub screen_memory: Option<Vec<NativeGlyph>>,
    pub symbol_depth: Vec<u32>,
    pub fg_depth: Vec<u32>,
    pub bg_depth: Vec<u32>,
    pub size: (u32, u32),
    pub limit: usize,
}

pub struct EngineInstance {
    pub event_loop: EventsLoop,
    pub gl_context: WindowedContext,
    pub pipeline: GlyphRenderer,
    pub gamepad: Gilrs,
}

impl Screen {
    pub fn is_ready(&self) -> bool {
        self.screen_memory.is_some()
    }

    pub fn glyphs_mut(&mut self) -> &mut Vec<NativeGlyph> {
        self.screen_memory.as_mut().unwrap()
    }

    pub fn glyphs(&self) -> &Vec<NativeGlyph> {
        self.screen_memory.as_ref().unwrap()
    }

    pub fn set_memory(&mut self, size: (u32, u32), memory: Vec<NativeGlyph>) {
        self.size = size;
        self.limit = (size.0 * size.1) as usize;

        self.symbol_depth = Vec::with_capacity(self.limit);
        for _i in 0..self.limit {
            self.symbol_depth.push(0);
        }

        self.fg_depth = Vec::with_capacity(self.limit);
        for _i in 0..self.limit {
            self.fg_depth.push(0);
        }

        self.bg_depth = Vec::with_capacity(self.limit);
        for _i in 0..self.limit {
            self.bg_depth.push(0);
        }

        self.screen_memory = Some(memory);
    }

    pub fn reinit_depth(&mut self) {
        for e in &mut self.symbol_depth {
            *e = 0;
        }
        for e in &mut self.fg_depth {
            *e = 0;
        }
        for e in &mut self.bg_depth {
            *e = 0;
        }
    }

    pub fn get_memory(&self) -> *const NativeGlyph {
        self.screen_memory.as_ref().unwrap().as_ptr()
    }

    pub fn len(&self) -> usize {
        self.screen_memory.as_ref().map(|v| v.len()).unwrap_or(0)
    }

    pub fn get_index_for_position(&self, position: (u32, u32)) -> u32 {
        position.1 * self.size.0 + position.0
    }
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
    use ChangeEvent::Write;
    use DataChange::Change;

    if let Some(Change(Write(p))) = snoop_for_data_changes() {
        Some(p)
    } else {
        None
    }
}

pub fn snoop_for_data_changes() -> Option<DataChange> {
    use notify::DebouncedEvent as E;
    use ChangeEvent::*;
    use ChangeEventError::*;
    use DataChange::*;

    match ENGINE_STATE
        .lock()
        .unwrap()
        .live_sync_state
        .watch_receiver
        .as_ref()
        .unwrap()
        .try_recv()
    {
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

pub(crate) fn start_engine(opts: EngineOptions) {
    if opts.seed.is_some() {
        rand::seed(opts.seed.unwrap());
    }

    let mut engine_state = ENGINE_STATE.lock().unwrap();
    engine_state.runtime_state.options = opts;

    let presentations = &mut engine_state.runtime_state.presentations;

    for maybe_file in std::fs::read_dir("resources/data/presentations/").unwrap() {
        if let Ok(file) = maybe_file {
            if let Ok(ron) = std::fs::read_to_string(file.path()) {
                let maybe_config: Result<Presentation, _> = ron::from_str(ron.as_str());
                if let Ok(config) = maybe_config {
                    presentations.insert(
                        file.path()
                            .file_stem()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .to_string(),
                        config,
                    );
                    println!("Added {:?} to presentations!", file.file_name());
                }
            }
        }
    }

    if presentations.is_empty() {
        panic!("No presentations, quitting!");
    }

    let mut live_sync = &mut engine_state.live_sync_state;
    let (tx, rx) = channel();
    live_sync.file_watcher = watcher(tx, Duration::from_secs(1)).ok();

    if let Ok(_) = live_sync
        .file_watcher
        .as_mut()
        .unwrap()
        .watch("resources/data", RecursiveMode::Recursive)
    {
        live_sync.watch_receiver = Some(rx);
    } else {
        panic!("WATCHER CAN'T WATCH THIS FOLDER!");
    }
}

pub fn force_redraw() {
    ENGINE_STATE.lock().unwrap().render_state.should_redraw = true;
}

#[allow(dead_code)]
pub fn reset_should_redraw() {
    ENGINE_STATE.lock().unwrap().render_state.should_redraw = false;
}

pub fn should_redraw() -> bool {
    ENGINE_STATE.lock().unwrap().render_state.should_redraw
}

pub fn should_quit() -> bool {
    ENGINE_STATE.lock().unwrap().runtime_state.should_quit
}

pub fn force_quit() {
    ENGINE_STATE.lock().unwrap().runtime_state.should_quit = true;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EngineOptions {
    pub vsync: bool,
    pub window_size: (u32, u32),
    pub title: String,
    pub presentation: String,
    pub seed: Option<u64>,
}

impl Default for EngineOptions {
    fn default() -> Self {
        match Path::exists(Path::new("resources/data/config.ron")) {
            true => ron::from_str(
                std::fs::read_to_string("resources/data/config.ron")
                    .unwrap()
                    .as_str(),
            )
            .unwrap(),
            false => {
                std::fs::create_dir_all("resources/data/").unwrap();
                std::fs::create_dir_all("resources/data/presentations").unwrap();
                std::fs::create_dir_all("resources/data/textures").unwrap();
                fs::write(
                    "resources/data/config.ron",
                    include_str!("cache/config.ron"),
                )
                .unwrap();

                fs::write(
                    "resources/data/presentations/8x8glyphs.ron",
                    include_str!("cache/8x8glyphs.ron"),
                )
                .unwrap();
                fs::write(
                    "resources/data/textures/8x8glyphs.png",
                    include_bytes!("cache/8x8glyphs.png"),
                )
                .unwrap();
                ron::from_str(
                    std::fs::read_to_string("resources/data/config.ron")
                        .unwrap()
                        .as_str(),
                )
                .unwrap()
            }
        }
    }
}
