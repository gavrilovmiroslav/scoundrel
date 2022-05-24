use std::time::{Instant};
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    pub static ref FRAME_TIME: Mutex<(u128, u128)> = Mutex::new((0u128, 0u128));
    pub static ref TIME: Mutex<Instant> = Mutex::new(Instant::now());
}

pub fn frame_start() {
    *TIME.lock().unwrap() = Instant::now();
}

pub fn frame_end() {
    let dt = TIME.lock().unwrap().elapsed();
    let mut t = FRAME_TIME.lock().unwrap();
    t.0 += dt.as_micros();
    t.1 += 1;
    if t.0 > 1_000_000 {
        println!("[FPS] {}", t.1);
        t.0 = 0;
        t.1 = 0;
    }
}