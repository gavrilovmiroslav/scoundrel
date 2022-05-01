use crate::core::engine;
use crate::core::point::Point;
use std::collections::HashSet;
use std::hash::Hash;

use crate::core::engine::INPUT_EVENTS;
use glutin::MouseButton;
use multimap::MultiMap;
use serde::*;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
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
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum GamepadState {
    ButtonPressed(GamepadButton),
    ButtonRepeated(GamepadButton),
    ButtonChanged(GamepadButton, i32),
    ButtonReleased(GamepadButton),
    AxisValueChanged(GamepadAxis, i32),
    Connected(GamepadID),
    Disconnected(GamepadID),
    Dropped(GamepadID),
}

#[derive(Debug, Hash, Ord, PartialOrd, PartialEq, Eq, Clone, Copy)]
#[repr(u32)]
pub enum Key {
    Key1,
    Key2,
    Key3,
    Key4,
    Key5,
    Key6,
    Key7,
    Key8,
    Key9,
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

    Snapshot,
    Scroll,
    Pause,

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

    Back,
    Return,
    Space,

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

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Input {
    Keyboard(Key),
    Gamepad(GamepadButton),
    Mouse(MouseButton),
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum WhenInput {
    Pressed(Input),
    Released(Input),
}

impl Into<(Input, InputState)> for WhenInput {
    fn into(self) -> (Input, InputState) {
        match self {
            WhenInput::Pressed(input) => (input, InputState::Pressed),
            WhenInput::Released(input) => (input, InputState::Released),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum InputState {
    None,
    Pressed,
    Released,
}

pub fn get_raw_input(input: Input) -> InputState {
    engine::INPUT_EVENTS
        .lock()
        .unwrap()
        .get(&input)
        .unwrap_or(&InputState::None)
        .clone()
}

pub fn get_mouse_position() -> Point {
    let point = engine::MOUSE_POSITIONS.lock().unwrap();

    Point::from((point.x, point.y))
}

pub enum Propagate {
    Flow,
    Stop,
}

pub trait InputContext<Action> {
    fn bubble_input(&self, action: &Action) -> (bool, Propagate);
}

pub struct InputStack<'a, Action> {
    pub input_stacks: Vec<&'a dyn InputContext<Action>>,
}

impl<'a, Action> InputStack<'a, Action> {
    pub fn new() -> Self {
        InputStack {
            input_stacks: vec![],
        }
    }

    pub fn get(&self, action: Action) -> bool {
        self.bubble_input(&action).0
    }

    pub fn push(&mut self, ic: &'a dyn InputContext<Action>) {
        self.input_stacks.push(ic);
    }

    pub fn pop(&mut self) {
        self.input_stacks.pop();
    }
}

impl<'a, Action> InputContext<Action> for InputStack<'a, Action> {
    fn bubble_input(&self, action: &Action) -> (bool, Propagate) {
        fn bubble_rec<'a, Action>(
            stack: &Vec<&'a dyn InputContext<Action>>,
            n: usize,
            action: &Action,
        ) -> (bool, Propagate) {
            match stack.get(n) {
                None => (false, Propagate::Stop),
                Some(input_context) => match input_context.bubble_input(action) {
                    (true, _) => (true, Propagate::Stop),
                    (false, Propagate::Flow) if n > 0 => bubble_rec(stack, n - 1, action),
                    (false, Propagate::Stop) => (false, Propagate::Stop),
                    _ => (false, Propagate::Stop),
                },
            }
        }

        let len = self.input_stacks.len() - 1;
        bubble_rec(&self.input_stacks, len, action)
    }
}

#[derive(Clone)]
pub struct AcceptingContext<Action>
where
    Action: Eq + Hash + Clone,
{
    mask: MultiMap<Action, (Input, InputState)>,
}

impl<Action> InputContext<Action> for AcceptingContext<Action>
where
    Action: Eq + Hash + Clone,
{
    fn bubble_input(&self, action: &Action) -> (bool, Propagate) {
        let raw_input = INPUT_EVENTS.lock().unwrap();
        if self.mask.contains_key(action) {
            for (input, state) in self.mask.get_vec(action).unwrap_or(&Vec::new()) {
                if raw_input.get(input).unwrap_or(&InputState::None) == state {
                    return (true, Propagate::Stop);
                }
            }
        }
        (false, Propagate::Flow)
    }
}

impl<Action> AcceptingContext<Action>
where
    Action: Eq + Hash + Clone,
{
    fn new() -> AcceptingContextBuilder<Action> {
        AcceptingContextBuilder {
            context: MultiMap::default(),
        }
    }

    pub fn add(action: Action, input: WhenInput) -> AcceptingContextBuilder<Action> {
        let mut builder = Self::new();
        builder.context.insert(action, input.into());
        builder.clone()
    }
}

#[derive(Clone)]
pub struct AcceptingContextBuilder<Action>
where
    Action: Eq + Hash + Clone,
{
    context: MultiMap<Action, (Input, InputState)>,
}

impl<Action> AcceptingContextBuilder<Action>
where
    Action: Eq + Hash + Clone,
{
    pub fn add(mut self, action: Action, input: WhenInput) -> Self {
        self.context.insert(action, input.into());
        self
    }

    pub fn build(self) -> AcceptingContext<Action> {
        AcceptingContext {
            mask: self.context.clone(),
        }
    }
}

#[derive(Clone)]
pub struct BlockingContext<Action>
where
    Action: Eq + Hash + Clone,
{
    mask: HashSet<Action>,
}

impl<Action> InputContext<Action> for BlockingContext<Action>
where
    Action: Eq + Hash + Clone,
{
    fn bubble_input(&self, action: &Action) -> (bool, Propagate) {
        if self.mask.contains(action) {
            (false, Propagate::Flow)
        } else {
            (false, Propagate::Stop)
        }
    }
}

impl<Action> BlockingContext<Action>
where
    Action: Eq + Hash + Clone,
{
    fn new() -> BlockingContextBuilder<Action> {
        BlockingContextBuilder {
            context: HashSet::new(),
        }
    }

    pub fn allow(action: Action) -> BlockingContextBuilder<Action> {
        let mut builder = Self::new();
        builder.context.insert(action);
        builder
    }

    pub fn empty() -> BlockingContext<Action> {
        Self::new().build()
    }
}

#[derive(Clone)]
pub struct BlockingContextBuilder<Action>
where
    Action: Eq + Hash + Clone,
{
    context: HashSet<Action>,
}

impl<Action> BlockingContextBuilder<Action>
where
    Action: Eq + Hash + Clone,
{
    pub fn allow(mut self, action: Action) -> Self {
        self.context.insert(action);
        self
    }

    pub fn build(self) -> BlockingContext<Action> {
        BlockingContext { mask: self.context }
    }
}
