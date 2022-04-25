use crate::core::engine;
use crate::core::keycodes::{GamepadState, Key, KeyStatus, MouseState};
use crate::core::point::Point;

pub fn poll_keyboard_events() -> Vec<(Key, KeyStatus)> {
    let mut queue = engine::KEYBOARD_EVENTS.lock().unwrap();
    let mut inputs = Vec::new();

    while let Some((state, status)) = queue.pop_front() {
        inputs.push((state, status));
    }

    inputs
}

pub fn poll_mouse_events() -> Vec<MouseState> {
    let mut queue = engine::MOUSE_EVENTS.lock().unwrap();
    let mut inputs = Vec::new();

    while let Some(state) = queue.pop_front() {
        inputs.push(state);
    }

    inputs
}

pub fn get_mouse_position() -> Point {
    let point = engine::MOUSE_POSITIONS.lock().unwrap();

    Point::from((point.x, point.y))
}

pub fn poll_gamepad_events() -> Vec<GamepadState> {
    let mut queue = engine::GAMEPAD_EVENTS.lock().unwrap();
    let mut inputs = Vec::new();

    while let Some(state) = queue.pop_front() {
        inputs.push(state);
    }

    inputs
}
