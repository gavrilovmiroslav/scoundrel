
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;
use scoundrel_common::shared_data::SharedData;

pub type ThreadState = JoinHandle<()>;

pub struct Engine {
    pub shared_data: SharedData,
    pub window_thread: Option<ThreadState>,
    pub logic_threads: Vec<ThreadState>,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            shared_data: SharedData::default(),
            window_thread: None,
            logic_threads: Vec::new(),
        }
    }

    pub fn run(&mut self, main_loop: fn(SharedData)) {
        self.window_thread = Some({
            let data = self.shared_data.clone();
            thread::spawn(move || main_loop(data))
        });
    }

    pub fn wait_until_shutdown(mut self) {
        self.window_thread.unwrap().join();
        println!("=| Killing main thread");
        *self.shared_data.should_quit.lock().unwrap() = true;

        while let Some(thread) = self.logic_threads.pop() {
            let id = thread.thread().id();
            println!("(| Killing thread {:?}", id);
            thread.join().unwrap();
            println!("|) Killed thread {:?}", id);
        }
    }

    pub fn add_logic(&mut self, name: &'static str, logic: fn(&SharedData)) {
        let kill_switch = self.shared_data.should_quit.clone();
        let data = self.shared_data.clone();

        let thread = thread::spawn(move || {
            loop {
                logic(&data);
                thread::sleep(Duration::from_millis(16));

                if *kill_switch.lock().unwrap() {
                    println!("|> Kill switch caught in {}", name);
                    break;
                }
            }
            println!("<| Done job in {}, quitting.", name);
        });

        self.logic_threads.push(thread);
    }
}