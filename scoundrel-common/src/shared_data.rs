use std::collections::VecDeque;
use std::sync::atomic::AtomicU64;
use std::time::Instant;
use crate::keycodes::KeyCode;
use crate::thread_safety::{thread_safe, ThreadSafe};

#[derive(Clone)]
pub struct SharedData {
    pub should_quit: ThreadSafe<bool>,
    pub stopwatch: ThreadSafe<Instant>,
    pub frame_counter: ThreadSafe<AtomicU64>,
    pub keyboard_events: ThreadSafe<VecDeque<KeyCode>>,
}

impl Default for SharedData {
    fn default() -> Self {
        SharedData {
            should_quit: thread_safe(false),
            stopwatch: thread_safe(Instant::now()),
            frame_counter: thread_safe(AtomicU64::new(0u64)),
            keyboard_events: thread_safe(VecDeque::default()),
        }
    }
}
