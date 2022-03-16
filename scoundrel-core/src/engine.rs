use std::thread;
use std::thread::JoinHandle;

use scoundrel_common::engine;
use scoundrel_common::engine_options::EngineOptions;

pub type ThreadState = JoinHandle<()>;

pub type PreSupportSystems = Vec<fn()>;
pub type PostSupportSystems = Vec<fn()>;

pub struct Engine {
    pub options: EngineOptions,
    pub pre_support_systems: PreSupportSystems,
    pub post_support_systems: PostSupportSystems,
}

impl Engine {
    pub fn new(options: EngineOptions) -> Engine {
        engine::start_engine(options.clone());

        Engine {
            options,
            pre_support_systems: Vec::new(),
            post_support_systems: Vec::new()
        }
    }

    pub fn run(&self, main_loop: fn(PreSupportSystems, PostSupportSystems)) {
        let pre = self.pre_support_systems.clone();
        let post = self.post_support_systems.clone();

        thread::spawn(move || {
            main_loop(pre, post)
        }).join();

        println!("=| Killing main thread");
    }
}