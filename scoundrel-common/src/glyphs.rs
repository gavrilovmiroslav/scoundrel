use std::collections::HashMap;
use crate::colors::{BLACK, Color, GRAY};
use crate::engine;
use crate::rascal::interpreter::{rascal_value_as_string, rascal_value_as_sym, RascalValue};
use crate::rascal::parser::SystemPrioritySize;

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
            foreground: *GRAY,
            background: *BLACK,
        }
    }
}

pub fn cls() {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        for glyph in screen.glyphs_mut() {
            glyph.symbol = '.' as u32;
            glyph.foreground = *GRAY;
            glyph.background = *BLACK;
        }
    }
}


pub fn print_field(field: &HashMap<usize, RascalValue>, fore: Color, back: Color, prio: SystemPrioritySize) {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        for (&index, v) in field {
            if index < 0 { return; }
            if index >= screen.limit { return; }

            if screen.symbol_depth[index] <= prio {
                screen.symbol_depth[index] = prio;
                let mut glyphs = screen.glyphs_mut();
                glyphs[index].symbol = rascal_value_as_sym(v) as u32;
            }
        }
    }
}

pub fn print_string_colors(position: (u32, u32), text: &str, fore: Color, back: Color, prio: SystemPrioritySize) {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        let mut index = screen.get_index_for_position(position).clone() as usize;

        for letter in text.chars() {
            if index < 0 { return; }
            if index >= screen.limit { return; }

            if screen.symbol_depth[index] <= prio {
                screen.symbol_depth[index] = prio;
                let mut glyphs = screen.glyphs_mut();
                glyphs[index].symbol = letter as u32;
            }

            if screen.fg_depth[index] <= prio {
                screen.fg_depth[index] = prio;
                let mut glyphs = screen.glyphs_mut();
                glyphs[index].foreground = fore;
            }

            if screen.bg_depth[index] <= prio {
                screen.bg_depth[index] = prio;
                let mut glyphs = screen.glyphs_mut();
                glyphs[index].background = back;
            }

            index += 1;
        }
    }
}

pub fn paint_tile(position: (u32, u32), fore: Option<Color>, back: Option<Color>, prio: SystemPrioritySize) {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        let mut index = screen.get_index_for_position(position).clone() as usize;
        if index < 0 { return; }
        if index >= screen.limit { return; }

        if fore.is_some() && screen.fg_depth[index] <= prio {
            screen.fg_depth[index] = prio;
            let mut glyphs = screen.glyphs_mut();
            glyphs[index].foreground = fore.unwrap();
        }

        if back.is_some() && screen.bg_depth[index] <= prio {
            screen.bg_depth[index] = prio;
            let mut glyphs = screen.glyphs_mut();
            glyphs[index].background = back.unwrap();
        }
    }
}

pub fn paint_all_tiles(fore: Option<Color>, back: Option<Color>) {
    let mut screen = engine::SCREEN.write().unwrap();
    if screen.is_ready() {
        if fore.is_some() {
            let fg = fore.unwrap();
            for glyph in screen.glyphs_mut() {
                glyph.foreground = fg;
            }
        }

        if back.is_some() {
            let bg = back.unwrap();
            for glyph in screen.glyphs_mut() {
                glyph.foreground = bg;
            }
        }
    }
}