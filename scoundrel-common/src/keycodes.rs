use serde::*;

/// Symbolic name for a keyboard key.
#[derive(Debug, Hash, Ord, PartialOrd, PartialEq, Eq, Clone, Copy)]
#[repr(u32)]
#[derive(Serialize, Deserialize)]
pub enum KeyState {
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
    NavigateForward, // also called "Prior"
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


pub fn key_action_to_name(action: ElementState) -> &'static str {
    match action {
        ElementState::Pressed => "Pressed",
        ElementState::Released => "Released",
    }
}

pub fn keystate_to_name(keystate: KeyState) -> &'static str {
    match keystate {
        KeyState::Caret => "Caret",
        KeyState::Copy => "Copy",
        KeyState::Paste => "Paste",
        KeyState::Cut => "Cut",
        KeyState::Key1 => "Number1",
        KeyState::Key2 => "Number2",
        KeyState::Key3 => "Number3",
        KeyState::Key4 => "Number4",
        KeyState::Key5 => "Number5",
        KeyState::Key6 => "Number6",
        KeyState::Key7 => "Number7",
        KeyState::Key8 => "Number8",
        KeyState::Key9 => "Number9",
        KeyState::Key0 => "Number0",
        KeyState::F1 => "F1",
        KeyState::F2 => "F2",
        KeyState::F3 => "F3",
        KeyState::F4 => "F4",
        KeyState::F5 => "F5",
        KeyState::F6 => "F6",
        KeyState::F7 => "F7",
        KeyState::F8 => "F8",
        KeyState::F9 => "F9",
        KeyState::F10 => "F10",
        KeyState::F11 => "F11",
        KeyState::F12 => "F12",
        KeyState::F13 => "F13",
        KeyState::F14 => "F14",
        KeyState::F15 => "F15",
        KeyState::F16 => "F16",
        KeyState::F17 => "F17",
        KeyState::F18 => "F18",
        KeyState::F19 => "F19",
        KeyState::F20 => "F20",
        KeyState::F21 => "F21",
        KeyState::F22 => "F22",
        KeyState::F23 => "F23",
        KeyState::F24 => "F24",
        KeyState::A => "A",
        KeyState::B => "B",
        KeyState::C => "C",
        KeyState::D => "D",
        KeyState::E => "E",
        KeyState::F => "F",
        KeyState::G => "G",
        KeyState::H => "H",
        KeyState::I => "I",
        KeyState::J => "J",
        KeyState::K => "K",
        KeyState::L => "L",
        KeyState::M => "M",
        KeyState::N => "N",
        KeyState::O => "O",
        KeyState::P => "P",
        KeyState::Q => "Q",
        KeyState::R => "R",
        KeyState::S => "S",
        KeyState::T => "T",
        KeyState::U => "U",
        KeyState::V => "V",
        KeyState::W => "W",
        KeyState::X => "X",
        KeyState::Y => "Y",
        KeyState::Z => "Z",
        KeyState::Escape => "Escape",
        KeyState::F1 => "F1",
        KeyState::F2 => "F2",
        KeyState::F3 => "F3",
        KeyState::F4 => "F4",
        KeyState::F5 => "F5",
        KeyState::F6 => "F6",
        KeyState::F7 => "F7",
        KeyState::F8 => "F8",
        KeyState::F9 => "F9",
        KeyState::F10 => "F10",
        KeyState::F11 => "F11",
        KeyState::F12 => "F12",
        KeyState::F13 => "F13",
        KeyState::F14 => "F14",
        KeyState::F15 => "F15",
        KeyState::Snapshot => "Snapshot",
        KeyState::Scroll => "Scroll",
        KeyState::Pause => "Pause",
        KeyState::Insert => "Insert",
        KeyState::Home => "Home",
        KeyState::Delete => "Delete",
        KeyState::End => "End",
        KeyState::PageDown => "PageDown",
        KeyState::PageUp => "PageUp",
        KeyState::Left => "Left",
        KeyState::Up => "Up",
        KeyState::Right => "Right",
        KeyState::Down => "Down",
        KeyState::Back => "Back",
        KeyState::Return => "Return",
        KeyState::Space => "Space",
        KeyState::Compose => "Compose",
        KeyState::Numlock => "Numlock",
        KeyState::Numpad0 => "Numpad0",
        KeyState::Numpad1 => "Numpad1",
        KeyState::Numpad2 => "Numpad2",
        KeyState::Numpad3 => "Numpad3",
        KeyState::Numpad4 => "Numpad4",
        KeyState::Numpad5 => "Numpad5",
        KeyState::Numpad6 => "Numpad6",
        KeyState::Numpad7 => "Numpad7",
        KeyState::Numpad8 => "Numpad8",
        KeyState::Numpad9 => "Numpad9",
        KeyState::AbntC1 => "AbntC1",
        KeyState::AbntC2 => "AbntC2",
        KeyState::Add => "Add",
        KeyState::Apostrophe => "Apostrophe",
        KeyState::Apps => "Apps",
        KeyState::At => "At",
        KeyState::Ax => "Ax",
        KeyState::Backslash => "Backslash",
        KeyState::Calculator => "Calculator",
        KeyState::Capital => "Capital",
        KeyState::Colon => "Colon",
        KeyState::Comma => "Comma",
        KeyState::Convert => "Convert",
        KeyState::Decimal => "Decimal",
        KeyState::Divide => "Divide",
        KeyState::Equals => "Equals",
        KeyState::Grave => "Grave",
        KeyState::Kana => "Kana",
        KeyState::Kanji => "Kanji",
        KeyState::LAlt => "LAlt",
        KeyState::LBracket => "LBracket",
        KeyState::LControl => "LControl",
        KeyState::LShift => "LShift",
        KeyState::LWin => "LWin",
        KeyState::Mail => "Mail",
        KeyState::MediaSelect => "MediaSelect",
        KeyState::MediaStop => "MediaStop",
        KeyState::Minus => "Minus",
        KeyState::Multiply => "Multiply",
        KeyState::Mute => "Mute",
        KeyState::MyComputer => "MyComputer",
        KeyState::NavigateForward => "NavigateForward",
        KeyState::NavigateBackward => "NavigateBackward",
        KeyState::NextTrack => "NextTrack",
        KeyState::NoConvert => "NoConvert",
        KeyState::NumpadComma => "NumpadComma",
        KeyState::NumpadEnter => "NumpadEnter",
        KeyState::NumpadEquals => "NumpadEquals",
        KeyState::OEM102 => "OEM102",
        KeyState::Period => "Period",
        KeyState::PlayPause => "PlayPause",
        KeyState::Power => "Power",
        KeyState::PrevTrack => "PrevTrack",
        KeyState::RAlt => "RAlt",
        KeyState::RBracket => "RBracket",
        KeyState::RControl => "RControl",
        KeyState::RShift => "RShift",
        KeyState::RWin => "RWin",
        KeyState::Semicolon => "Semicolon",
        KeyState::Slash => "Slash",
        KeyState::Sleep => "Sleep",
        KeyState::Stop => "Stop",
        KeyState::Subtract => "Subtract",
        KeyState::Sysrq => "Sysrq",
        KeyState::Tab => "Tab",
        KeyState::Underline => "Underline",
        KeyState::Unlabeled => "Unlabeled",
        KeyState::VolumeDown => "VolumeDown",
        KeyState::VolumeUp => "VolumeUp",
        KeyState::Wake => "Wake",
        KeyState::WebBack => "WebBack",
        KeyState::WebFavorites => "WebFavorites",
        KeyState::WebForward => "WebForward",
        KeyState::WebHome => "WebHome",
        KeyState::WebRefresh => "WebRefresh",
        KeyState::WebSearch => "WebSearch",
        KeyState::WebStop => "WebStop",
        KeyState::Yen => "Yen",
    }
}

unsafe impl Send for KeyState {}

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
    pub logo: bool
}

/// Describes the input state of a key.
#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum ElementState {
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
    pub state: ElementState,
}

impl MouseState {
    pub fn new(bt: MouseButton, st: ElementState) -> MouseState {
        MouseState{ button: bt, state: st }
    }
}