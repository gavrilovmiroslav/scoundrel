use crate::core::point::Point;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

use crate::core::engine::ENGINE_STATE;
use crate::graphics::window::EngineInstance;
use glutin::MouseButton;
use multimap::MultiMap;
use ron::de::ErrorCode;
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

#[derive(Debug, Hash, Ord, PartialOrd, PartialEq, Eq, Clone, Copy, Deserialize)]
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

#[derive(PartialEq, Eq, Hash, Clone, Deserialize, Debug)]
pub enum Input {
    Keyboard(Key),
    Gamepad(GamepadButton),
    Mouse(MouseButton),
}

#[derive(PartialEq, Eq, Hash, Clone, Deserialize, Debug)]
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

pub fn poll_inputs(instance: &mut EngineInstance) {
    instance.poll_inputs();
}

pub fn get_raw_input(input: Input) -> InputState {
    ENGINE_STATE
        .lock()
        .unwrap()
        .input_state
        .input_events
        .get(&input)
        .unwrap_or(&InputState::None)
        .clone()
}

pub fn get_mouse_position() -> Point {
    ENGINE_STATE.lock().unwrap().input_state.mouse_position
}

// Input Contexts

#[derive(Deserialize, Debug)]
pub enum ContextDescriptor {
    Accept(HashMap<String, Vec<WhenInput>>),
    BlockExcept(Vec<String>),
}

pub fn get_bindable_input_context(input_file: &str) -> Result<ContextDescriptor, ErrorCode> {
    let inputs_path = Rc::new(format!("resources/input_bindings/{}.ron", input_file));
    match std::fs::read_to_string(inputs_path.clone().as_str()) {
        Ok(input_content) => match ron::from_str::<ContextDescriptor>(input_content.as_str()) {
            Ok(desc) => Ok(desc),
            Err(err) => Err(err.code),
        },
        Err(_) => Err(ErrorCode::Io(format!("File {} not found.", inputs_path))),
    }
}

pub enum Propagate {
    Flow,
    Stop,
}

pub trait InputContext<Action> {
    fn bubble_input(&self, action: &Action) -> (bool, Propagate);
}

pub struct InputStack<Action> {
    pub input_stacks: Vec<Rc<dyn InputContext<Action>>>,
}

pub trait PushContext<T> {
    fn push(&mut self, ic: T);
}

pub trait BindableInputAction: Clone + Eq + Hash {
    fn from_str<S: AsRef<str>>(s: S) -> Option<Self>;
}

impl<Action> InputStack<Action> {
    pub fn new() -> Self {
        InputStack {
            input_stacks: vec![],
        }
    }

    pub fn get(&self, action: Action) -> bool {
        self.bubble_input(&action).0
    }

    pub fn pop(&mut self) {
        self.input_stacks.pop();
    }
}

impl<Action> PushContext<ContextDescriptor> for InputStack<Action>
where
    Action: 'static + BindableInputAction,
{
    fn push(&mut self, ic: ContextDescriptor) {
        match ic {
            ContextDescriptor::Accept(ctx) => {
                let mut context = AcceptingContext::raw();
                for (s, vec) in ctx {
                    match Action::from_str(s.clone()) {
                        Some(act) => {
                            for input in &vec {
                                context.mask.insert(act.clone(), input.clone().into());
                            }
                        }
                        None => {
                            println!(
                                "Bad binding found: couldn't parse action {} in accepting context",
                                s
                            );
                        }
                    }
                }
                self.input_stacks.push(Rc::new(context));
            }

            ContextDescriptor::BlockExcept(vec) => {
                let mut context = BlockingContext::raw();
                for s in vec {
                    match Action::from_str(s.clone()) {
                        Some(act) => {
                            context.mask.insert(act.clone());
                        }
                        None => {
                            println!(
                                "Bad binding found: couldn't parse action {} in blocking context",
                                s
                            );
                        }
                    }
                }
                self.input_stacks.push(Rc::new(context));
            }
        }
    }
}

impl<Action> PushContext<AcceptingContext<Action>> for InputStack<Action>
where
    Action: Eq + Hash + Clone + 'static,
{
    fn push(&mut self, ic: AcceptingContext<Action>) {
        self.input_stacks.push(Rc::new(ic));
    }
}

impl<Action> PushContext<BlockingContext<Action>> for InputStack<Action>
where
    Action: Eq + Hash + Clone + 'static,
{
    fn push(&mut self, ic: BlockingContext<Action>) {
        self.input_stacks.push(Rc::new(ic));
    }
}

impl<Action> InputContext<Action> for InputStack<Action> {
    fn bubble_input(&self, action: &Action) -> (bool, Propagate) {
        fn bubble_rec<Action>(
            stack: &Vec<Rc<dyn InputContext<Action>>>,
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
        let raw_input = &ENGINE_STATE.lock().unwrap().input_state.input_events;
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
    fn raw() -> AcceptingContext<Action> {
        AcceptingContext {
            mask: MultiMap::default(),
        }
    }

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
    pub(crate) context: MultiMap<Action, (Input, InputState)>,
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
    pub(crate) mask: HashSet<Action>,
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
    fn raw() -> BlockingContext<Action> {
        BlockingContext {
            mask: HashSet::default(),
        }
    }

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
