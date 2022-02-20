use std::sync::atomic::Ordering;
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

use scoundrel_common::engine_context::EngineContext;
use scoundrel_common::engine_options::EngineOptions;

pub type ThreadState = JoinHandle<()>;

pub struct Engine {
    pub engine_context: EngineContext,
    pub logic: Vec<fn(&EngineContext)>,
}

impl Engine {
    pub fn new(options: EngineOptions) -> Engine {
        Engine {
            engine_context: EngineContext::default_with_opts(options),
            logic: Vec::new(),
        }
    }

    pub fn run(&self, main_loop: fn(EngineContext)) {
        let logics = self.logic.clone();
        let engine_context = self.engine_context.clone();
        thread::spawn(move || {
            while !engine_context.should_quit.load(Ordering::Acquire) {
                if !engine_context.should_redraw.load(Ordering::Acquire) {
                    for logic in &logics {
                        logic(&engine_context);
                    }

                    engine_context.should_redraw.store(true, Ordering::Release);
                }
                thread::sleep(Duration::from_millis(1));
            }
        });

        let engine_context = self.engine_context.clone();
        thread::spawn(move || {
            main_loop(engine_context);
        }).join();

        println!("=| Killing main thread");
    }
}