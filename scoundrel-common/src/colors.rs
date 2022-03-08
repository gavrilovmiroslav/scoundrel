use lazy_static::*;
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

impl From<(u8, u8, u8)> for Color {
    fn from((h, s, v): (u8, u8, u8)) -> Self {
        Color::new(h, s, v)
    }
}

impl Color {
    pub fn new(h: u8, s: u8, v: u8) -> Self {
        Color { hue: h, sat: s, val: v, eff: 0 }
    }

    pub fn rand() -> Color {
        let mut rng = thread_rng();
        Color { hue: rng.gen_range(0..255), sat: rng.gen_range(0..255), val: rng.gen_range(0..255), eff: 0 }
    }
}

lazy_static! {
    pub static ref WHITE: Color = Color::new(0, 0, 255);
    pub static ref GRAY: Color = Color::new(0, 0, 128);
    pub static ref BLACK: Color = Color::new(0, 0, 0);
}