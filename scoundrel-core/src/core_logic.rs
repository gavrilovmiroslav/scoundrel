use std::time::Instant;

use scoundrel_common::engine;
use scoundrel_common::engine::WORLD;
use scoundrel_common::glyphs::cls;
use scoundrel_common::keycodes::{key_action_to_name, keystate_to_name};
use scoundrel_common::rascal::interpreter::{num, text};
use scoundrel_common::rascal::world::TriggerEvent;

pub fn clear_screen() {
    cls();
}

pub fn pass_input_events_to_rascal() {
    let mut queue = engine::KEYBOARD_EVENTS.lock().unwrap();
    while let Some((key, action)) = queue.pop_front() {
        let mut world = WORLD.lock().unwrap();
        world.trigger_event("KeyPress", vec![ text(keystate_to_name(key)), text(key_action_to_name(action)) ]);
    }
    drop(queue);

    let mut mouse_cursor = engine::MOUSE_POSITIONS.lock().unwrap();
    let mut world = WORLD.lock().unwrap();
    let (x, y) = (mouse_cursor.x, mouse_cursor.y);
    if (x >= 0 && x < 64) && (x >= 0 && y < 48) {
        world.trigger_event("MouseMove", vec![num(x), num(y)]);
    }
    drop(mouse_cursor);
}

pub fn track_fps() {
    let mut stopwatch = engine::STOPWATCH.lock().unwrap();
    let mut frame_counter = engine::FRAME_COUNTER.lock().unwrap();

    let time = stopwatch.elapsed().as_millis();

    if time >= 1000 {
        *stopwatch = Instant::now();
        frame_counter.cache();
    }

    let mut world = WORLD.lock().unwrap();
    world.trigger_event("FPS", vec![ num(frame_counter.cached() as i32) ]);
}