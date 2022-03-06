use std::thread;
use std::thread::JoinHandle;

use scoundrel_common::engine;
use scoundrel_common::engine_options::EngineOptions;

pub type ThreadState = JoinHandle<()>;

pub struct Engine {
    pub support_systems: Vec<fn()>,
}

impl Engine {
    pub fn new(options: EngineOptions) -> Engine {
        engine::start_engine(options);

        Engine { support_systems: Vec::new(), }
    }

    pub fn run(&self, main_loop: fn(Vec<fn()>)) {
        let logics = self.support_systems.clone();

        thread::spawn(move || {
            main_loop(logics)
        }).join();

        println!("=| Killing main thread");
    }
}