use crate::colors::Color;

#[derive(Copy, Clone)]
#[no_mangle]
#[repr(C)]
pub struct Glyph {
    pub symbol: u32,
    pub foreground: Color,
    pub background: Color,
}

impl Default for Glyph {
    fn default() -> Self {
        Glyph {
            symbol: '.' as u32,
            foreground: Color::rand(),
            background: Color::new(0, 0, 0),
        }
    }
}