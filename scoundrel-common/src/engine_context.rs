use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::vec_deque::Iter;
use std::collections::{HashMap, VecDeque};
use std::ops::Deref;
use std::sync::{Arc, Mutex, RwLock};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Instant;
use crate::colors::Color;

use crate::engine_options::EngineOptions;
use crate::glyphs::Glyph;
use crate::keycodes::{KeyState, MouseState};
use crate::point::Point;
use crate::presentation::Presentation;

#[derive(Clone, Default)]
pub struct FrameCounter {
    active_frame_count: Arc<AtomicU64>,
    last_cached_count: Arc<AtomicU64>,
}

impl FrameCounter {
    pub fn tick(&mut self) {
        self.active_frame_count.fetch_add(1, Ordering::Acquire);
    }

    pub fn cache(&mut self) {
        self.last_cached_count.store(self.active_frame_count.fetch_min(0, Ordering::Relaxed), Ordering::Release);
    }

    pub fn cached(&self) -> u64 {
        self.last_cached_count.load(Ordering::Acquire)
    }
}

pub struct Screen {
    pub screen_memory: Option<Vec<Glyph>>,
    pub size: (u32, u32),
    pub should_redraw: bool,
}

impl Screen {
    pub fn new() -> Screen {
        Screen { screen_memory: None, size: (0, 0), should_redraw: true }
    }

    pub fn is_ready(&self) -> bool {
        self.screen_memory.is_some()
    }

    pub fn force_redraw(&mut self) {
        self.should_redraw = true;
    }

    pub fn reset_redraw(&mut self) {
        self.should_redraw = false;
    }

    pub fn glyphs_mut(&mut self) -> &mut Vec<Glyph>{
        self.screen_memory.as_mut().unwrap()
    }

    pub fn glyphs(&self) -> &Vec<Glyph>{
        self.screen_memory.as_ref().unwrap()
    }

    pub fn set_memory(&mut self, size: (u32, u32), memory: Vec<Glyph>) {
        self.size = size;
        self.screen_memory = Some(memory);
    }

    pub fn get_memory(&self) -> *const Glyph {
        self.screen_memory.as_ref().unwrap().as_ptr()
    }

    pub fn len(&self) -> usize {
        self.screen_memory.as_ref().map(|v| v.len()).unwrap_or(0)
    }

    pub fn get_index_for_position(&self, position: (u32, u32)) -> u32 {
        position.1 * self.size.0 + position.0
    }
}

#[derive(Clone)]
pub struct EngineContext {
    pub options: EngineOptions,
    pub presentations: Arc<Mutex<HashMap<String, Presentation>>>,
    pub should_quit: Arc<AtomicBool>,
    pub stopwatch: Arc<Mutex<Instant>>,
    pub frame_counter: Arc<Mutex<FrameCounter>>,
    pub keyboard_events: Arc<Mutex<VecDeque<KeyState>>>,
    pub mouse_events: Arc<Mutex<VecDeque<MouseState>>>,
    pub mouse_position: Arc<Mutex<Point>>,
    pub screen: Arc<RwLock<Screen>>,
    pub should_redraw: Arc<AtomicBool>,
}

impl Default for EngineContext {
    fn default() -> Self {
        EngineContext {
            options: EngineOptions::default(),
            presentations: Arc::new(Mutex::new(HashMap::new())),
            should_quit: Arc::new(AtomicBool::new(false)),
            stopwatch: Arc::new(Mutex::new(Instant::now())),
            frame_counter: Arc::new(Mutex::new(FrameCounter::default())),
            keyboard_events: Arc::new(Mutex::new(VecDeque::default())),
            mouse_events: Arc::new(Mutex::new(VecDeque::default())),
            mouse_position: Arc::new(Mutex::new((0, 0).into())),
            screen: Arc::new(RwLock::new(Screen::new())),
            should_redraw: Arc::new(AtomicBool::new(true)),
        }
    }
}

impl EngineContext {
    pub fn default_with_opts(opts: EngineOptions) -> EngineContext {
        let mut context = EngineContext::default();
        context.options = opts;

        for maybe_file in std::fs::read_dir("./data/presentations/").unwrap() {
            if let Ok(file) = maybe_file {
                if let Ok(ron) = std::fs::read_to_string(file.path()) {
                    let maybe_config: Result<Presentation, _> = ron::from_str(ron.as_str());
                    if let Ok(config) = maybe_config {
                        let mut presentations = context.presentations.lock().unwrap();
                        presentations.insert(file.path().file_stem().unwrap().to_str().unwrap().to_string(), config);
                        println!("Added {:?} to presentations!", file.file_name());
                    }
                }
            }
        }

        if context.presentations.lock().unwrap().is_empty() {
            panic!("No presentations, quitting!");
        }

        context
    }

    pub fn print_string(&self, position: (u32, u32), text: &str) {
        let mut screen = self.screen.write().unwrap();
        if screen.is_ready() {
            let mut index = screen.get_index_for_position(position).clone() as usize;
            let mut glyphs = screen.glyphs_mut();
            for i in 0..text.len() { glyphs[index + i].symbol = 0; }
            for letter in text.chars() {
                glyphs[index].symbol = letter as u32;
                glyphs[index].foreground = Color::new(255, 255, 255);
                glyphs[index].background = Color::new(0, 0, 0);
                index += 1;
            }
            screen.should_redraw = true;
        }
    }

    pub fn print_string_color(&self, position: (u32, u32), text: &str, fore: Color) {
        let mut screen = self.screen.write().unwrap();
        if screen.is_ready() {
            let mut index = screen.get_index_for_position(position).clone() as usize;
            let mut glyphs = screen.glyphs_mut();
            for i in 0..text.len() { glyphs[index + i].symbol = 0; }
            for letter in text.chars() {
                glyphs[index].symbol = letter as u32;
                glyphs[index].foreground = fore;
                index += 1;
            }
            screen.should_redraw = true;
        }
    }

    pub fn print_string_colors(&self, position: (u32, u32), text: &str, fore: Color, back: Color) {
        let mut screen = self.screen.write().unwrap();
        if screen.is_ready() {
            let mut index = screen.get_index_for_position(position).clone() as usize;
            let mut glyphs = screen.glyphs_mut();
            for i in 0..text.len() { glyphs[index + i].symbol = 0; }
            for letter in text.chars() {
                glyphs[index].symbol = letter as u32;
                glyphs[index].foreground = fore;
                glyphs[index].background = back;
                index += 1;
            }
            screen.should_redraw = true;
        }
    }
}
