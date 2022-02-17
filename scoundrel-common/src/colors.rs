
use rand::{Rng, thread_rng};

#[derive(Copy, Clone)]
#[no_mangle]
#[repr(C)]
pub struct Color {
    pub hue: u32,
    pub sat: u32,
    pub val: u32,
}

impl Color {
    pub fn rand() -> Color {
        Color {
            hue: thread_rng().gen_range(0..255),
            sat: thread_rng().gen_range(0..255),
            val: thread_rng().gen_range(0..255),
        }
    }
}