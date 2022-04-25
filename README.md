# scoundrel
A roguelike engine with devious intent!

# Simple example

This is the smallest, minimal example of a "game" in Scoundrel:

```rust
pub fn main() {
    start_with_systems(
        vec![], // systems that work _before_ rendering
        vec![], // systems that work _after_ rendering
        windowing::window_event_loop,
    );
}
```

It does nothing, however, so let's add a bit to make it better:
```rust
pub fn draw_character() {
    print_string_colors((10, 10), "HELLO WORLD", *WHITE, *BLACK, 1);
}

pub fn main() {
    start_with_systems(
        vec![draw_character],
        vec![],
        windowing::window_event_loop,
    );
}
```

To add input, you can poll keyboard, mouse and even gamepad input:

```rust
pub fn exit_on_escape() {
    for (key, status) in poll_keyboard_events() {
        if status == KeyStatus::Released && key == Key::Escape {
            force_quit();
        }
    }
}

pub fn draw_character() {
    print_string_colors((10, 10), "HELLO WORLD", *WHITE, *BLACK, 1);
}

pub fn main() {
    start_with_systems(
        vec![draw_character],
        vec![exit_on_escape],
        windowing::window_event_loop,
    );
}
```
