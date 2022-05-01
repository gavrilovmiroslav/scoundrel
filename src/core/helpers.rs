use crate::core::engine;
use std::time::Instant;

pub fn track_fps() {
    let mut stopwatch = engine::STOPWATCH.lock().unwrap();
    let mut frame_counter = engine::FRAME_COUNTER.lock().unwrap();

    let time = stopwatch.elapsed().as_millis();

    if time >= 1000 {
        *stopwatch = Instant::now();
        frame_counter.cache();
    }
}
