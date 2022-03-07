use std::time::Instant;

use scoundrel_common::engine;
use scoundrel_common::engine::{force_quit, WORLD};
use scoundrel_common::keycodes::{key_action_to_name, keystate_to_name};
use scoundrel_common::rascal::vm::{num, text};
use scoundrel_common::rascal::world::{AddComponent, world_contains_event};

pub fn pass_input_events_to_rascal() {
    let mut queue = engine::KEYBOARD_EVENTS.lock().unwrap();
    while let Some((key, action)) = queue.pop_front() {
        let mut world = WORLD.lock().unwrap();
        let entity = world.create_entity();
        world.add_component(entity, "KeyPress", vec![ text(keystate_to_name(key)), text(key_action_to_name(action)) ]);
    }
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
    let entity = world.create_entity();
    world.add_component(entity, "FPS", vec![ num(frame_counter.cached() as i32) ]);
}

pub fn exit_game_from_script() {
    if world_contains_event("ExitGame") {
        force_quit();
    }
}
