use serde::*;

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum GamepadAxis {
    LeftStickX,
    LeftStickY,
    LeftZ,
    RightStickX,
    RightStickY,
    RightZ,
    DPadX,
    DPadY,
    Unknown,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum GamepadButton {
    South,
    East,
    North,
    West,
    C,
    Z,
    LeftTrigger,
    LeftTrigger2,
    RightTrigger,
    RightTrigger2,
    Select,
    Start,
    Mode,
    LeftThumb,
    RightThumb,
    DPadUp,
    DPadDown,
    DPadLeft,
    DPadRight,
    Unknown,
}

pub type GamepadID = usize;

#[derive(Debug, Clone, Copy)]
#[repr(u32)]
#[derive(Serialize, Deserialize)]
pub enum GamepadState {
    ButtonPressed(GamepadID, GamepadButton),
    ButtonRepeated(GamepadID, GamepadButton),
    ButtonChanged(GamepadID, GamepadButton, f32),
    ButtonReleased(GamepadID, GamepadButton),
    AxisValueChanged(GamepadID, GamepadAxis, f32),
    Connected(GamepadID),
    Disconnected(GamepadID),
    Dropped(GamepadID),
}

/// Symbolic name for a keyboard key.
#[derive(Debug, Hash, Ord, PartialOrd, PartialEq, Eq, Clone, Copy)]
#[repr(u32)]
#[derive(Serialize, Deserialize)]
pub enum Key {
    /// The '1' key over the letters.
    Key1,
    /// The '2' key over the letters.
    Key2,
    /// The '3' key over the letters.
    Key3,
    /// The '4' key over the letters.
    Key4,
    /// The '5' key over the letters.
    Key5,
    /// The '6' key over the letters.
    Key6,
    /// The '7' key over the letters.
    Key7,
    /// The '8' key over the letters.
    Key8,
    /// The '9' key over the letters.
    Key9,
    /// The '0' key over the 'O' and 'P' keys.
    Key0,

    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,

    /// The Escape key, next to F1.
    Escape,

    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,

    /// Print Screen/SysRq.
    Snapshot,
    /// Scroll Lock.
    Scroll,
    /// Pause/Break key, next to Scroll lock.
    Pause,

    /// `Insert`, next to Backspace.
    Insert,
    Home,
    Delete,
    End,
    PageDown,
    PageUp,

    Left,
    Up,
    Right,
    Down,

    /// The Backspace key, right over Enter.
    Back,
    /// The Enter key.
    Return,
    /// The space bar.
    Space,

    /// The "Compose" key on Linux.
    Compose,

    Caret,

    Numlock,
    Numpad0,
    Numpad1,
    Numpad2,
    Numpad3,
    Numpad4,
    Numpad5,
    Numpad6,
    Numpad7,
    Numpad8,
    Numpad9,

    AbntC1,
    AbntC2,
    Add,
    Apostrophe,
    Apps,
    At,
    Ax,
    Backslash,
    Calculator,
    Capital,
    Colon,
    Comma,
    Convert,
    Decimal,
    Divide,
    Equals,
    Grave,
    Kana,
    Kanji,
    LAlt,
    LBracket,
    LControl,
    LShift,
    LWin,
    Mail,
    MediaSelect,
    MediaStop,
    Minus,
    Multiply,
    Mute,
    MyComputer,
    NavigateForward,  // also called "Prior"
    NavigateBackward, // also called "Next"
    NextTrack,
    NoConvert,
    NumpadComma,
    NumpadEnter,
    NumpadEquals,
    OEM102,
    Period,
    PlayPause,
    Power,
    PrevTrack,
    RAlt,
    RBracket,
    RControl,
    RShift,
    RWin,
    Semicolon,
    Slash,
    Sleep,
    Stop,
    Subtract,
    Sysrq,
    Tab,
    Underline,
    Unlabeled,
    VolumeDown,
    VolumeUp,
    Wake,
    WebBack,
    WebFavorites,
    WebForward,
    WebHome,
    WebRefresh,
    WebSearch,
    WebStop,
    Yen,
    Copy,
    Paste,
    Cut,
}

pub fn key_action_to_name(action: KeyStatus) -> &'static str {
    match action {
        KeyStatus::Pressed => "Pressed",
        KeyStatus::Released => "Released",
    }
}

pub fn keystate_to_name(keystate: Key) -> &'static str {
    match keystate {
        Key::Caret => "Caret",
        Key::Copy => "Copy",
        Key::Paste => "Paste",
        Key::Cut => "Cut",
        Key::Key1 => "Number1",
        Key::Key2 => "Number2",
        Key::Key3 => "Number3",
        Key::Key4 => "Number4",
        Key::Key5 => "Number5",
        Key::Key6 => "Number6",
        Key::Key7 => "Number7",
        Key::Key8 => "Number8",
        Key::Key9 => "Number9",
        Key::Key0 => "Number0",
        Key::F1 => "F1",
        Key::F2 => "F2",
        Key::F3 => "F3",
        Key::F4 => "F4",
        Key::F5 => "F5",
        Key::F6 => "F6",
        Key::F7 => "F7",
        Key::F8 => "F8",
        Key::F9 => "F9",
        Key::F10 => "F10",
        Key::F11 => "F11",
        Key::F12 => "F12",
        Key::F13 => "F13",
        Key::F14 => "F14",
        Key::F15 => "F15",
        Key::F16 => "F16",
        Key::F17 => "F17",
        Key::F18 => "F18",
        Key::F19 => "F19",
        Key::F20 => "F20",
        Key::F21 => "F21",
        Key::F22 => "F22",
        Key::F23 => "F23",
        Key::F24 => "F24",
        Key::A => "A",
        Key::B => "B",
        Key::C => "C",
        Key::D => "D",
        Key::E => "E",
        Key::F => "F",
        Key::G => "G",
        Key::H => "H",
        Key::I => "I",
        Key::J => "J",
        Key::K => "K",
        Key::L => "L",
        Key::M => "M",
        Key::N => "N",
        Key::O => "O",
        Key::P => "P",
        Key::Q => "Q",
        Key::R => "R",
        Key::S => "S",
        Key::T => "T",
        Key::U => "U",
        Key::V => "V",
        Key::W => "W",
        Key::X => "X",
        Key::Y => "Y",
        Key::Z => "Z",
        Key::Escape => "Escape",
        Key::Snapshot => "Snapshot",
        Key::Scroll => "Scroll",
        Key::Pause => "Pause",
        Key::Insert => "Insert",
        Key::Home => "Home",
        Key::Delete => "Delete",
        Key::End => "End",
        Key::PageDown => "PageDown",
        Key::PageUp => "PageUp",
        Key::Left => "Left",
        Key::Up => "Up",
        Key::Right => "Right",
        Key::Down => "Down",
        Key::Back => "Back",
        Key::Return => "Return",
        Key::Space => "Space",
        Key::Compose => "Compose",
        Key::Numlock => "Numlock",
        Key::Numpad0 => "Numpad0",
        Key::Numpad1 => "Numpad1",
        Key::Numpad2 => "Numpad2",
        Key::Numpad3 => "Numpad3",
        Key::Numpad4 => "Numpad4",
        Key::Numpad5 => "Numpad5",
        Key::Numpad6 => "Numpad6",
        Key::Numpad7 => "Numpad7",
        Key::Numpad8 => "Numpad8",
        Key::Numpad9 => "Numpad9",
        Key::AbntC1 => "AbntC1",
        Key::AbntC2 => "AbntC2",
        Key::Add => "Add",
        Key::Apostrophe => "Apostrophe",
        Key::Apps => "Apps",
        Key::At => "At",
        Key::Ax => "Ax",
        Key::Backslash => "Backslash",
        Key::Calculator => "Calculator",
        Key::Capital => "Capital",
        Key::Colon => "Colon",
        Key::Comma => "Comma",
        Key::Convert => "Convert",
        Key::Decimal => "Decimal",
        Key::Divide => "Divide",
        Key::Equals => "Equals",
        Key::Grave => "Grave",
        Key::Kana => "Kana",
        Key::Kanji => "Kanji",
        Key::LAlt => "LAlt",
        Key::LBracket => "LBracket",
        Key::LControl => "LControl",
        Key::LShift => "LShift",
        Key::LWin => "LWin",
        Key::Mail => "Mail",
        Key::MediaSelect => "MediaSelect",
        Key::MediaStop => "MediaStop",
        Key::Minus => "Minus",
        Key::Multiply => "Multiply",
        Key::Mute => "Mute",
        Key::MyComputer => "MyComputer",
        Key::NavigateForward => "NavigateForward",
        Key::NavigateBackward => "NavigateBackward",
        Key::NextTrack => "NextTrack",
        Key::NoConvert => "NoConvert",
        Key::NumpadComma => "NumpadComma",
        Key::NumpadEnter => "NumpadEnter",
        Key::NumpadEquals => "NumpadEquals",
        Key::OEM102 => "OEM102",
        Key::Period => "Period",
        Key::PlayPause => "PlayPause",
        Key::Power => "Power",
        Key::PrevTrack => "PrevTrack",
        Key::RAlt => "RAlt",
        Key::RBracket => "RBracket",
        Key::RControl => "RControl",
        Key::RShift => "RShift",
        Key::RWin => "RWin",
        Key::Semicolon => "Semicolon",
        Key::Slash => "Slash",
        Key::Sleep => "Sleep",
        Key::Stop => "Stop",
        Key::Subtract => "Subtract",
        Key::Sysrq => "Sysrq",
        Key::Tab => "Tab",
        Key::Underline => "Underline",
        Key::Unlabeled => "Unlabeled",
        Key::VolumeDown => "VolumeDown",
        Key::VolumeUp => "VolumeUp",
        Key::Wake => "Wake",
        Key::WebBack => "WebBack",
        Key::WebFavorites => "WebFavorites",
        Key::WebForward => "WebForward",
        Key::WebHome => "WebHome",
        Key::WebRefresh => "WebRefresh",
        Key::WebSearch => "WebSearch",
        Key::WebStop => "WebStop",
        Key::Yen => "Yen",
    }
}

unsafe impl Send for Key {}

/// Represents the current state of the keyboard modifiers
///
/// Each field of this struct represents a modifier and is `true` if this modifier is active.
#[derive(Default, Debug, Clone, Copy)]
pub struct ModifiersState {
    /// The "shift" key
    pub shift: bool,
    /// The "control" key
    pub ctrl: bool,
    /// The "alt" key
    pub alt: bool,
    /// The "logo" key
    ///
    /// This is the "windows" key on PC and "command" key on Mac.
    pub logo: bool,
}

/// Describes the input state of a key.
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum KeyStatus {
    Pressed,
    Released,
}

/// Describes a button of a mouse controller.
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum MouseButton {
    Left,
    Right,
    Middle,
    Other(u8),
}

pub struct MouseState {
    pub button: MouseButton,
    pub state: KeyStatus,
}

impl MouseState {
    pub fn new(bt: MouseButton, st: KeyStatus) -> MouseState {
        MouseState {
            button: bt,
            state: st,
        }
    }
}
