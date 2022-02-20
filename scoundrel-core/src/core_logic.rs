use std::sync::atomic::Ordering;
use std::time::Instant;
use rand::{Rng, thread_rng};
use scoundrel_common::colors::Color;

use scoundrel_common::engine_context::EngineContext;

pub fn create_input_checker(engine_context: &EngineContext) {
    if let Ok(mut queue) = engine_context.keyboard_events.lock() {
        if let Some(key) = queue.pop_front() {
            let mut screen = engine_context.screen.write().unwrap();
            if screen.is_ready() {
                for mut glyph in screen.glyphs_mut() {
                    glyph.symbol = thread_rng().gen_range(0..255);
                    glyph.foreground.hue = ((glyph.foreground.hue as u16 + 1) % 255) as u8;
                }
            }
        }
    }
}

pub fn create_fps_tracker(engine_context: &EngineContext) {
    let mut stopwatch = engine_context.stopwatch.lock().unwrap();
    let mut frame_counter = engine_context.frame_counter.lock().unwrap();

    let time = stopwatch.elapsed().as_millis();

    if time >= 1000 {
        *stopwatch = Instant::now();
        frame_counter.cache();
    }

    let message = format!("FPS: {:.0}", frame_counter.cached());
    engine_context.print_string((1, 0), message.as_str());
}
