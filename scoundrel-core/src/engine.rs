use std::sync::atomic::Ordering;
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;
use scoundrel_common::engine;

use scoundrel_common::engine_options::EngineOptions;

pub type ThreadState = JoinHandle<()>;

pub struct Engine {
    pub logic: Vec<fn()>,
}

impl Engine {
    pub fn new(options: EngineOptions) -> Engine {
        engine::start_engine(options);

        Engine { logic: Vec::new(), }
    }

    pub fn run(&self, main_loop: fn()) {
        let logics = self.logic.clone();
        thread::spawn(move || {
            while !engine::should_quit() {
                if !engine::should_redraw() {
                    for logic in &logics { logic(); }
                    engine::force_redraw();
                }
                thread::sleep(Duration::from_millis(1));
            }
        });

        thread::spawn(move || main_loop()).join();

        println!("=| Killing main thread");
    }
}