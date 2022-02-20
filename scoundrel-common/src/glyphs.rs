use crate::colors::Color;
use crate::engine;

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

pub fn print_glyph(position: (u32, u32), glyph: Glyph) {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        let mut index = screen.get_index_for_position(position).clone() as usize;
        let mut glyphs = screen.glyphs_mut();
        glyphs[index] = glyph;
        screen.should_redraw = true;
    }
}

pub fn print_string(position: (u32, u32), text: &str) {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        let mut index = screen.get_index_for_position(position).clone() as usize;
        let mut glyphs = screen.glyphs_mut();
        for i in 0..text.len() { glyphs[index + i].symbol = 0; }
        for letter in text.chars() {
            glyphs[index].symbol = letter as u32;
            glyphs[index].foreground = Color::new(255, 255, 255);
            glyphs[index].background = Color::new(0, 0, 0);
            index += 1;
        }
        screen.should_redraw = true;
    }
}

pub fn print_string_color(position: (u32, u32), text: &str, fore: Color) {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        let mut index = screen.get_index_for_position(position).clone() as usize;
        let mut glyphs = screen.glyphs_mut();
        for i in 0..text.len() { glyphs[index + i].symbol = 0; }
        for letter in text.chars() {
            glyphs[index].symbol = letter as u32;
            glyphs[index].foreground = fore;
            index += 1;
        }
        screen.should_redraw = true;
    }
}

pub fn print_string_colors(position: (u32, u32), text: &str, fore: Color, back: Color) {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        let mut index = screen.get_index_for_position(position).clone() as usize;
        let mut glyphs = screen.glyphs_mut();
        for i in 0..text.len() { glyphs[index + i].symbol = 0; }
        for letter in text.chars() {
            glyphs[index].symbol = letter as u32;
            glyphs[index].foreground = fore;
            glyphs[index].background = back;
            index += 1;
        }
        screen.should_redraw = true;
    }
}