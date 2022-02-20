use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::vec_deque::Iter;
use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use std::sync::{Arc, Mutex, RwLock};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Instant;
use crate::colors::Color;

use crate::engine_options::EngineOptions;
use crate::glyphs::Glyph;
use crate::keycodes::{KeyState, MouseState};
use crate::point::Point;
use crate::presentation::Presentation;

use lazy_static;
use lazy_static::*;

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
    pub size: (u32, u32),
    pub should_redraw: bool,
}

impl Screen {
    pub fn new() -> Screen {
        Screen { screen_memory: None, size: (0, 0), should_redraw: true }
    }

    pub fn is_ready(&self) -> bool {
        self.screen_memory.is_some()
    }

    pub fn force_redraw(&mut self) {
        self.should_redraw = true;
    }

    pub fn reset_redraw(&mut self) {
        self.should_redraw = false;
    }

    pub fn glyphs_mut(&mut self) -> &mut Vec<Glyph>{
        self.screen_memory.as_mut().unwrap()
    }

    pub fn glyphs(&self) -> &Vec<Glyph>{
        self.screen_memory.as_ref().unwrap()
    }

    pub fn set_memory(&mut self, size: (u32, u32), memory: Vec<Glyph>) {
        self.size = size;
        self.screen_memory = Some(memory);
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
    pub static ref KEYBOARD_EVENTS: Arc<Mutex<VecDeque<KeyState>>> = Arc::new(Mutex::new(VecDeque::default()));
    pub static ref MOUSE_EVENTS: Arc<Mutex<VecDeque<MouseState>>> = Arc::new(Mutex::new(VecDeque::default()));
    pub static ref MOUSE_POSITIONS: Arc<Mutex<Point>> = Arc::new(Mutex::new((0, 0).into()));
    pub static ref SCREEN: Arc<RwLock<Screen>> = Arc::new(RwLock::new(Screen::new()));
    pub static ref SHOULD_REDRAW: Arc<AtomicBool> = Arc::new(AtomicBool::new(true));
}

pub fn start_engine(opts: EngineOptions) {
    *ENGINE_OPTIONS.lock().unwrap() = opts;

    let mut presentations = PRESENTATIONS.lock().unwrap();

    for maybe_file in std::fs::read_dir("./data/presentations/").unwrap() {
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
}

pub fn should_quit() -> bool {
    SHOULD_QUIT.load(Ordering::Acquire)
}

pub fn force_quit() {
    SHOULD_QUIT.store(true, Ordering::Release);
}

pub fn should_redraw() -> bool {
    SHOULD_REDRAW.load(Ordering::Acquire)
}

pub fn force_redraw() {
    SHOULD_REDRAW.store(true, Ordering::Release);
}

pub fn clean_redraw() {
    SHOULD_REDRAW.store(false, Ordering::Release);
}

