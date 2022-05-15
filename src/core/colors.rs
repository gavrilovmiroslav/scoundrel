#[derive(Copy, Clone, Debug)]
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
        Color {
            hue: h,
            sat: s,
            val: v,
            eff: 0,
        }
    }

    pub fn rand() -> Color {
        Color {
            hue: fastrand::u8(0..255),
            sat: fastrand::u8(0..255),
            val: fastrand::u8(0..255),
            eff: 0,
        }
    }

    pub const WHITE: Color = Color {
        hue: 0,
        sat: 0,
        val: 255,
        eff: 0,
    };
    pub const GRAY: Color = Color {
        hue: 0,
        sat: 0,
        val: 128,
        eff: 0,
    };
    pub const BLACK: Color = Color {
        hue: 0,
        sat: 0,
        val: 0,
        eff: 0,
    };
    pub const GREEN: Color = Color {
        hue: 64,
        sat: 255,
        val: 255,
        eff: 0,
    };
}
