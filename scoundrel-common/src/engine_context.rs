use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::vec_deque::Iter;
use std::collections::VecDeque;
use std::ops::Deref;
use std::sync::{Arc, Mutex, RwLock};
use std::sync::atomic::{AtomicBool, AtomicU64};
use std::time::Instant;
use crate::keycodes::KeyCode;

#[derive(Clone)]
pub struct EngineContext {
    pub should_quit: Arc<AtomicBool>,
    pub stopwatch: Arc<Mutex<Instant>>,
    pub frame_counter: Arc<AtomicU64>,
    pub keyboard_events: Arc<Mutex<VecDeque<KeyCode>>>,
}

impl Default for EngineContext {
    fn default() -> Self {
        EngineContext {
            should_quit: Arc::new(AtomicBool::new(false)),
            stopwatch: Arc::new(Mutex::new(Instant::now())),
            frame_counter: Arc::new(AtomicU64::new(0u64)),
            keyboard_events: Arc::new(Mutex::new(VecDeque::default())),
        }
    }
}
