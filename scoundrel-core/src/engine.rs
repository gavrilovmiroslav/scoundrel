use std::sync::atomic::Ordering;
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;

use scoundrel_common::engine_context::EngineContext;
use scoundrel_common::engine_options::EngineOptions;

pub type ThreadState = JoinHandle<()>;

pub struct Engine {
    pub engine_context: EngineContext,
    pub window_thread: Option<ThreadState>,
    pub logic_threads: Vec<ThreadState>,
}

impl Engine {
    pub fn new(options: EngineOptions) -> Engine {
        Engine {
            engine_context: EngineContext::default_with_opts(options),
            window_thread: None,
            logic_threads: Vec::new(),
        }
    }

    pub fn run(&mut self, main_loop: fn(EngineContext)) {
        self.window_thread = Some({
            let data = self.engine_context.clone();
            thread::spawn(move || main_loop(data))
        });
    }

    pub fn wait_until_shutdown(mut self) {
        self.window_thread.unwrap().join().unwrap();
        println!("=| Killing main thread");
        self.engine_context.should_quit.store(true, Ordering::SeqCst);

        while let Some(thread) = self.logic_threads.pop() {
            let id = thread.thread().id();
            println!("(| Killing thread {:?}", id);
            thread.join().unwrap();
            println!("|) Killed thread {:?}", id);
        }

        std::process::exit(0);
    }

    pub fn add_logic(&mut self, name: &'static str, logic: fn(&EngineContext)) {
        let kill_switch = self.engine_context.should_quit.clone();
        let data = self.engine_context.clone();

        let thread = thread::spawn(move || {
            loop {
                logic(&data);
                thread::sleep(Duration::from_millis(1));

                if kill_switch.load(Ordering::SeqCst) {
                    println!("|> Kill switch caught in {}", name);
                    break;
                }
            }
            println!("<| Done job in {}, quitting.", name);
        });

        self.logic_threads.push(thread);
    }
}