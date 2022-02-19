use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::vec_deque::Iter;
use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use std::sync::{Arc, Mutex, RwLock};
use std::sync::atomic::{AtomicBool, AtomicU64};
use std::time::Instant;

use crate::engine_options::EngineOptions;
use crate::glyphs::Glyph;
use crate::keycodes::{KeyState, MouseState};
use crate::point::Point;
use crate::presentation::Presentation;

#[derive(Clone)]
pub struct EngineContext {
    pub options: EngineOptions,
    pub presentations: Arc<Mutex<HashMap<String, Presentation>>>,
    pub should_quit: Arc<AtomicBool>,
    pub stopwatch: Arc<Mutex<Instant>>,
    pub frame_counter: Arc<RwLock<u64>>,
    pub keyboard_events: Arc<Mutex<VecDeque<KeyState>>>,
    pub mouse_events: Arc<Mutex<VecDeque<MouseState>>>,
    pub mouse_position: Arc<Mutex<Point>>,
    pub screen_memory: Arc<RwLock<Vec<Glyph>>>,
}

impl Default for EngineContext {
    fn default() -> Self {
        EngineContext {
            options: EngineOptions::default(),
            presentations: Arc::new(Mutex::new(HashMap::new())),
            should_quit: Arc::new(AtomicBool::new(false)),
            stopwatch: Arc::new(Mutex::new(Instant::now())),
            frame_counter: Arc::new(RwLock::new(0u64)),
            keyboard_events: Arc::new(Mutex::new(VecDeque::default())),
            mouse_events: Arc::new(Mutex::new(VecDeque::default())),
            mouse_position: Arc::new(Mutex::new((0, 0).into())),
            screen_memory: Arc::new(RwLock::new(Vec::new())),
        }
    }
}

impl EngineContext {
    pub fn default_with_opts(opts: EngineOptions) -> EngineContext {
        let mut context = EngineContext::default();
        context.options = opts;

        for maybe_file in std::fs::read_dir("./data/presentations/").unwrap() {
            if let Ok(file) = maybe_file {
                if let Ok(ron) = std::fs::read_to_string(file.path()) {
                    let maybe_config: Result<Presentation, _> = ron::from_str(ron.as_str());
                    if let Ok(config) = maybe_config {
                        let mut presentations = context.presentations.lock().unwrap();
                        presentations.insert(file.path().file_stem().unwrap().to_str().unwrap().to_string(), config);
                        println!("Added {:?} to presentations!", file.file_name());
                    }
                }
            }
        }

        if context.presentations.lock().unwrap().is_empty() {
            panic!("No presentations, quitting!");
        }

        context
    }
}
