use gilrs::{Axis, Button};
use scoundrel_core::keycodes::{GamepadAxis, GamepadButton};

pub fn gilrs_to_axis(axis: Axis) -> GamepadAxis {
    match axis {
        Axis::LeftStickX => GamepadAxis::LeftStickX,
        Axis::LeftStickY => GamepadAxis::LeftStickY,
        Axis::LeftZ => GamepadAxis::LeftZ,
        Axis::RightStickX => GamepadAxis::RightStickX,
        Axis::RightStickY => GamepadAxis::RightStickY,
        Axis::RightZ => GamepadAxis::RightZ,
        Axis::DPadX => GamepadAxis::DPadX,
        Axis::DPadY => GamepadAxis::DPadY,
        Axis::Unknown => GamepadAxis::Unknown,
    }
}

pub fn gilrs_to_button(button: Button) -> GamepadButton {
    match button {
        Button::South => GamepadButton::South,
        Button::East => GamepadButton::East,
        Button::North => GamepadButton::North,
        Button::West => GamepadButton::West,
        Button::C => GamepadButton::C,
        Button::Z => GamepadButton::Z,
        Button::LeftTrigger => GamepadButton::LeftTrigger,
        Button::LeftTrigger2 => GamepadButton::LeftTrigger2,
        Button::RightTrigger => GamepadButton::RightTrigger,
        Button::RightTrigger2 => GamepadButton::RightTrigger2,
        Button::Select => GamepadButton::Select,
        Button::Start => GamepadButton::Start,
        Button::Mode => GamepadButton::Mode,
        Button::LeftThumb => GamepadButton::LeftThumb,
        Button::RightThumb => GamepadButton::RightThumb,
        Button::DPadUp => GamepadButton::DPadUp,
        Button::DPadDown => GamepadButton::DPadDown,
        Button::DPadLeft => GamepadButton::DPadLeft,
        Button::DPadRight => GamepadButton::DPadRight,
        Button::Unknown => GamepadButton::Unknown,
    }
}
