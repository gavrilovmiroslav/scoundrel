use crate::core::point::Point;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;
use glfw::ffi;

use crate::core::engine::ENGINE_STATE;
use crate::engine::EngineInstance;
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

#[repr(i32)]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
#[derive(Serialize, Deserialize)]
pub enum Key {
    Space = ffi::KEY_SPACE,
    Apostrophe = ffi::KEY_APOSTROPHE,
    Comma = ffi::KEY_COMMA,
    Minus = ffi::KEY_MINUS,
    Period = ffi::KEY_PERIOD,
    Slash = ffi::KEY_SLASH,
    Num0 = ffi::KEY_0,
    Num1 = ffi::KEY_1,
    Num2 = ffi::KEY_2,
    Num3 = ffi::KEY_3,
    Num4 = ffi::KEY_4,
    Num5 = ffi::KEY_5,
    Num6 = ffi::KEY_6,
    Num7 = ffi::KEY_7,
    Num8 = ffi::KEY_8,
    Num9 = ffi::KEY_9,
    Semicolon = ffi::KEY_SEMICOLON,
    Equal = ffi::KEY_EQUAL,
    A = ffi::KEY_A,
    B = ffi::KEY_B,
    C = ffi::KEY_C,
    D = ffi::KEY_D,
    E = ffi::KEY_E,
    F = ffi::KEY_F,
    G = ffi::KEY_G,
    H = ffi::KEY_H,
    I = ffi::KEY_I,
    J = ffi::KEY_J,
    K = ffi::KEY_K,
    L = ffi::KEY_L,
    M = ffi::KEY_M,
    N = ffi::KEY_N,
    O = ffi::KEY_O,
    P = ffi::KEY_P,
    Q = ffi::KEY_Q,
    R = ffi::KEY_R,
    S = ffi::KEY_S,
    T = ffi::KEY_T,
    U = ffi::KEY_U,
    V = ffi::KEY_V,
    W = ffi::KEY_W,
    X = ffi::KEY_X,
    Y = ffi::KEY_Y,
    Z = ffi::KEY_Z,
    LeftBracket = ffi::KEY_LEFT_BRACKET,
    Backslash = ffi::KEY_BACKSLASH,
    RightBracket = ffi::KEY_RIGHT_BRACKET,
    GraveAccent = ffi::KEY_GRAVE_ACCENT,
    World1 = ffi::KEY_WORLD_1,
    World2 = ffi::KEY_WORLD_2,

    Escape = ffi::KEY_ESCAPE,
    Enter = ffi::KEY_ENTER,
    Tab = ffi::KEY_TAB,
    Backspace = ffi::KEY_BACKSPACE,
    Insert = ffi::KEY_INSERT,
    Delete = ffi::KEY_DELETE,
    Right = ffi::KEY_RIGHT,
    Left = ffi::KEY_LEFT,
    Down = ffi::KEY_DOWN,
    Up = ffi::KEY_UP,
    PageUp = ffi::KEY_PAGE_UP,
    PageDown = ffi::KEY_PAGE_DOWN,
    Home = ffi::KEY_HOME,
    End = ffi::KEY_END,
    CapsLock = ffi::KEY_CAPS_LOCK,
    ScrollLock = ffi::KEY_SCROLL_LOCK,
    NumLock = ffi::KEY_NUM_LOCK,
    PrintScreen = ffi::KEY_PRINT_SCREEN,
    Pause = ffi::KEY_PAUSE,
    F1 = ffi::KEY_F1,
    F2 = ffi::KEY_F2,
    F3 = ffi::KEY_F3,
    F4 = ffi::KEY_F4,
    F5 = ffi::KEY_F5,
    F6 = ffi::KEY_F6,
    F7 = ffi::KEY_F7,
    F8 = ffi::KEY_F8,
    F9 = ffi::KEY_F9,
    F10 = ffi::KEY_F10,
    F11 = ffi::KEY_F11,
    F12 = ffi::KEY_F12,
    F13 = ffi::KEY_F13,
    F14 = ffi::KEY_F14,
    F15 = ffi::KEY_F15,
    F16 = ffi::KEY_F16,
    F17 = ffi::KEY_F17,
    F18 = ffi::KEY_F18,
    F19 = ffi::KEY_F19,
    F20 = ffi::KEY_F20,
    F21 = ffi::KEY_F21,
    F22 = ffi::KEY_F22,
    F23 = ffi::KEY_F23,
    F24 = ffi::KEY_F24,
    F25 = ffi::KEY_F25,
    Kp0 = ffi::KEY_KP_0,
    Kp1 = ffi::KEY_KP_1,
    Kp2 = ffi::KEY_KP_2,
    Kp3 = ffi::KEY_KP_3,
    Kp4 = ffi::KEY_KP_4,
    Kp5 = ffi::KEY_KP_5,
    Kp6 = ffi::KEY_KP_6,
    Kp7 = ffi::KEY_KP_7,
    Kp8 = ffi::KEY_KP_8,
    Kp9 = ffi::KEY_KP_9,
    KpDecimal = ffi::KEY_KP_DECIMAL,
    KpDivide = ffi::KEY_KP_DIVIDE,
    KpMultiply = ffi::KEY_KP_MULTIPLY,
    KpSubtract = ffi::KEY_KP_SUBTRACT,
    KpAdd = ffi::KEY_KP_ADD,
    KpEnter = ffi::KEY_KP_ENTER,
    KpEqual = ffi::KEY_KP_EQUAL,
    LeftShift = ffi::KEY_LEFT_SHIFT,
    LeftControl = ffi::KEY_LEFT_CONTROL,
    LeftAlt = ffi::KEY_LEFT_ALT,
    LeftSuper = ffi::KEY_LEFT_SUPER,
    RightShift = ffi::KEY_RIGHT_SHIFT,
    RightControl = ffi::KEY_RIGHT_CONTROL,
    RightAlt = ffi::KEY_RIGHT_ALT,
    RightSuper = ffi::KEY_RIGHT_SUPER,
    Menu = ffi::KEY_MENU,
    Unknown = ffi::KEY_UNKNOWN,
}

#[repr(i32)]
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
#[derive(Serialize, Deserialize)]
pub enum MouseButton {
    Button1,
    Button2,
    Button3,
    Button4,
    Button5,
    Button6,
    Button7,
    Button8,
}

impl From<glfw::MouseButton> for MouseButton {
    fn from(mb: glfw::MouseButton) -> Self {
        match mb {
            glfw::MouseButton::Button1 => MouseButton::Button1,
            glfw::MouseButton::Button2 => MouseButton::Button2,
            glfw::MouseButton::Button3 => MouseButton::Button3,
            glfw::MouseButton::Button4 => MouseButton::Button4,
            glfw::MouseButton::Button5 => MouseButton::Button5,
            glfw::MouseButton::Button6 => MouseButton::Button6,
            glfw::MouseButton::Button7 => MouseButton::Button7,
            glfw::MouseButton::Button8 => MouseButton::Button8,
        }
    }
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
                let p = raw_input.get(input).unwrap_or(&InputState::None);
                if p == state {
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
