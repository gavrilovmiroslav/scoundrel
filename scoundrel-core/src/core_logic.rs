use std::sync::atomic::Ordering;
use std::time::Instant;

use scoundrel_common::engine_context::EngineContext;

pub fn create_input_checker(data: &EngineContext) {
    if let Ok(mut queue) = data.keyboard_events.lock() {
        if let Some(key) = queue.pop_front() {
            println!("The key pressed is {:?}", key);
        }
    }
}

pub fn create_fps_tracker(data: &EngineContext) {
    let mut stopwatch = data.stopwatch.lock().unwrap();
    let time = stopwatch.elapsed().as_millis();

    if time >= 1000 {
        *stopwatch = Instant::now();
        println!("FPS: {:.0}", *data.frame_counter.read().unwrap() as f64 / time as f64 * 1000.0);
        *data.frame_counter.write().unwrap() = 0;
    }
}
