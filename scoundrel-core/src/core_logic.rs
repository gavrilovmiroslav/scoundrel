use std::time::Instant;

use rand::{Rng, thread_rng};

use scoundrel_common::{engine, glyphs};
use scoundrel_common::engine::{force_quit, WORLD};
use scoundrel_common::keycodes::{key_action_to_name, keystate_to_name};
use scoundrel_common::rascal::vm::text;
use scoundrel_common::rascal::world::{AddComponent, world_contains_event};

pub fn pass_input_events_to_rascal() {
    let mut queue = engine::KEYBOARD_EVENTS.lock().unwrap();
    while let Some((key, action)) = queue.pop_front() {
        let mut world = WORLD.lock().unwrap();
        let entity = world.create_entity();
        world.add_component(entity, "KeyPress", vec![ text(keystate_to_name(key)), text(key_action_to_name(action)) ]);
        drop(world);

        let mut screen = engine::SCREEN.write().unwrap();
        if screen.is_ready() {
            for mut glyph in screen.glyphs_mut() {
                glyph.symbol = thread_rng().gen_range(0..255);
                glyph.foreground.hue = ((glyph.foreground.hue as u16 + 1) % 255) as u8;
            }
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
    }

    let message = format!("FPS: {:.0}", frame_counter.cached());
    glyphs::print_string((1, 0), message.as_str());
}

pub fn create_exit_game() {
    if world_contains_event("ExitGame") {
        force_quit();
    }
}
