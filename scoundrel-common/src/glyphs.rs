use crate::colors::Color;

#[derive(Copy, Clone)]
#[no_mangle]
#[repr(C)]
pub struct Glyph {
    pub symbol: u32,
    pub foreground: Color,
    pub background: Color,
}