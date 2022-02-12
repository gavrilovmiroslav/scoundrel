use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::vec_deque::Iter;
use std::collections::VecDeque;
use std::ops::Deref;
use std::sync::{Arc, Mutex, RwLock};
use std::sync::atomic::{AtomicBool, AtomicU64};
use std::time::Instant;

use crate::engine_options::EngineOptions;
use crate::keycodes::{KeyState, MouseState};
use crate::point::Point;

#[derive(Clone)]
pub struct EngineContext {
    pub options: EngineOptions,
    pub should_quit: Arc<AtomicBool>,
    pub stopwatch: Arc<Mutex<Instant>>,
    pub frame_counter: Arc<RwLock<u64>>,
    pub keyboard_events: Arc<Mutex<VecDeque<KeyState>>>,
    pub mouse_events: Arc<Mutex<VecDeque<MouseState>>>,
    pub mouse_position: Arc<Mutex<Point>>,
}

impl Default for EngineContext {
    fn default() -> Self {
        EngineContext {
            options: EngineOptions::default(),
            should_quit: Arc::new(AtomicBool::new(false)),
            stopwatch: Arc::new(Mutex::new(Instant::now())),
            frame_counter: Arc::new(RwLock::new(0u64)),
            keyboard_events: Arc::new(Mutex::new(VecDeque::default())),
            mouse_events: Arc::new(Mutex::new(VecDeque::default())),
            mouse_position: Arc::new(Mutex::new((0, 0).into())),
        }
    }
}

impl EngineContext {
    pub fn default_with_opts(opts: EngineOptions) -> EngineContext {
        let mut context = EngineContext::default();
        context.options = opts;
        context
    }
}
