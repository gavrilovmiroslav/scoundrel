use std::sync::atomic::Ordering;
use std::time::Instant;
use rand::{Rng, thread_rng};
use scoundrel_common::colors::Color;
use scoundrel_common::{engine, glyphs};

pub fn create_input_checker() {
    let mut queue = engine::KEYBOARD_EVENTS.lock().unwrap();
    while let Some(key) = queue.pop_front() {
        let mut screen = engine::SCREEN.write().unwrap();
        if screen.is_ready() {
            for mut glyph in screen.glyphs_mut() {
                glyph.symbol = thread_rng().gen_range(0..255);
                glyph.foreground.hue = ((glyph.foreground.hue as u16 + 1) % 255) as u8;
            }
            engine::force_redraw();
        }
    }
}

pub fn create_fps_tracker() {
    let mut stopwatch = engine::STOPWATCH.lock().unwrap();
    let mut frame_counter = engine::FRAME_COUNTER.lock().unwrap();

    let time = stopwatch.elapsed().as_millis();

    if time >= 1000 {
        *stopwatch = Instant::now();
        frame_counter.cache();
        engine::force_redraw();
    }

    let message = format!("FPS: {:.0}", frame_counter.cached());
    glyphs::print_string((1, 0), message.as_str());
}
