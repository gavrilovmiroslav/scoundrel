use std::sync::atomic::Ordering;
use std::time::Instant;
use scoundrel_common::shared_data::SharedData;

pub fn create_input_checker(data: &SharedData) {
    if let Ok(mut queue) = data.keyboard_events.lock() {
        if let Some(key) = queue.pop_front() {
            println!("The key pressed is {:?}", key);
        }
    }
}

pub fn create_fps_tracker(data: &SharedData) {
    let mut stopwatch = data.stopwatch.lock().unwrap();
    let time = stopwatch.elapsed().as_millis();

    if time >= 1000 {
        *stopwatch = Instant::now();
        let frames = data.frame_counter.lock().unwrap().fetch_min(0u64, Ordering::Relaxed);
        println!("FPS: {:.0}", frames as f64 / time as f64 * 1000.0);
    }
}
