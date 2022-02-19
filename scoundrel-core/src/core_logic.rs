use std::sync::atomic::Ordering;
use std::time::Instant;
use rand::{Rng, thread_rng};
use scoundrel_common::colors::Color;

use scoundrel_common::engine_context::EngineContext;

pub fn create_stress_renderer(engine_context: &EngineContext) {
    let mut screen_memory = engine_context.screen_memory.read().unwrap().clone();

    for mut glyph in &mut screen_memory {
        glyph.symbol = thread_rng().gen_range(0..255);
        glyph.foreground.hue = ((glyph.foreground.hue as u16 + 1) % 255) as u8;
    }

    *engine_context.screen_memory.write().unwrap() = screen_memory;
}

pub fn create_input_checker(engine_context: &EngineContext) {
    if let Ok(mut queue) = engine_context.keyboard_events.lock() {
        if let Some(key) = queue.pop_front() {
            println!("The key pressed is {:?}", key);
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
        println!("FPS: {}", frame_counter.cached());
    }

    let message = format!("FPS: {:.0}", frame_counter.cached());
    {
        if let Ok(mut screen_memory) = engine_context.screen_memory.try_write() {
            if screen_memory.len() > 0 {
                let mut index = 1;
                for i in 0..10 { screen_memory[i].symbol = 0; }
                for letter in message.chars() {
                    screen_memory[index].symbol = letter as u32;
                    screen_memory[index].foreground = Color::new(255, 255, 255);
                    screen_memory[index].background = Color::new(0, 0, 0);
                    index += 1;
                }
            }
        }
    }
}
