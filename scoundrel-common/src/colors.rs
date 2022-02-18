
use rand::{Rng, thread_rng};

#[derive(Copy, Clone)]
#[no_mangle]
#[repr(C)]
pub struct Color {
    pub hue: u8,
    pub sat: u8,
    pub val: u8,
    pub eff: u8,
}

impl Color {
    pub fn new(h: u8, s: u8, v: u8) -> Color {
        Color { hue: h, sat: s, val: v, eff: 255 }
    }
}