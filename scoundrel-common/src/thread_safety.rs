use std::sync::{Arc, Mutex};

pub type ThreadSafe<T> = Arc<Mutex<T>>;
pub fn thread_safe<T>(t: T) -> ThreadSafe<T> {
    Arc::new(Mutex::new(t))
}
