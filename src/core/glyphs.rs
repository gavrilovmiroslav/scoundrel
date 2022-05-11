use crate::core::colors::{Color, BLACK, GRAY, WHITE};
use crate::core::engine::ENGINE_STATE;

#[derive(Copy, Clone)]
#[repr(C)]
pub struct NativeGlyph {
    pub symbol: u32,
    pub foreground: Color,
    pub background: Color,
}

impl Default for NativeGlyph {
    fn default() -> Self {
        NativeGlyph {
            symbol: ' ' as u32,
            foreground: *GRAY,
            background: *BLACK,
        }
    }
}

#[derive(Clone, Copy)]
pub enum GlyphTint {
    None,
    Fore(Color),
    Back(Color),
    BackFore(Color, Color),
}

impl Default for GlyphTint {
    fn default() -> Self {
        GlyphTint::None
    }
}

#[derive(Clone, Copy, Default)]
pub struct Glyph {
    pub symbol: Option<char>,
    pub tint: GlyphTint,
}

pub fn clear_screen() {
    let screen = &mut ENGINE_STATE.lock().unwrap().render_state.screen;
    if screen.is_ready() {
        for glyph in screen.glyphs_mut() {
            glyph.symbol = ' ' as u32;
            glyph.foreground = *GRAY;
            glyph.background = *BLACK;
        }
    }
}

pub fn print_char<P: Into<(u32, u32)>>(position: P, chr: char, depth: u32) {
    print_string_colors(position, chr.to_string(), *WHITE, *BLACK, depth);
}

pub fn print_char_colors<P: Into<(u32, u32)>>(
    position: P,
    chr: char,
    fore: Color,
    back: Color,
    depth: u32,
) {
    print_string_colors(position, chr.to_string(), fore, back, depth);
}

pub fn print_string<S: AsRef<str>, P: Into<(u32, u32)>>(position: P, text: S, depth: u32) {
    print_string_colors(position, text, *WHITE, *BLACK, depth);
}

pub fn print_string_colors<S: AsRef<str>, P: Into<(u32, u32)>>(
    position: P,
    text: S,
    fore: Color,
    back: Color,
    depth: u32,
) {
    let window_size = ENGINE_STATE.lock().unwrap().render_state.screen_size;
    let screen = &mut ENGINE_STATE.lock().unwrap().render_state.screen;
    if screen.is_ready() {
        let mut p = position.into();
        if p.0 >= window_size.0 {
            return;
        }

        let mut index = screen.get_index_for_position(p).clone() as usize;

        for letter in text.as_ref().chars() {
            if p.0 >= window_size.0 {
                return;
            }

            p.0 += 1;

            if index >= screen.limit {
                return;
            }

            if screen.symbol_depth[index] <= depth {
                screen.symbol_depth[index] = depth;
                let glyphs = screen.glyphs_mut();
                glyphs[index].symbol = letter as u32;
            }

            if screen.fg_depth[index] <= depth {
                screen.fg_depth[index] = depth;
                let glyphs = screen.glyphs_mut();
                glyphs[index].foreground = fore;
            }

            if screen.bg_depth[index] <= depth {
                screen.bg_depth[index] = depth;
                let glyphs = screen.glyphs_mut();
                glyphs[index].background = back;
            }

            index += 1;
        }
    }
}

pub fn paint_tile<P: Into<(u32, u32)>>(
    position: P,
    fore: Option<Color>,
    back: Option<Color>,
    depth: u32,
) {
    let window_size = ENGINE_STATE
        .lock()
        .unwrap()
        .runtime_state
        .options
        .window_size
        .clone();

    let screen = &mut ENGINE_STATE.lock().unwrap().render_state.screen;
    if screen.is_ready() {
        let p = position.into();
        let index = screen.get_index_for_position(p).clone() as usize;

        if p.0 >= window_size.0 {
            return;
        }

        if index >= screen.limit {
            return;
        }

        if fore.is_some() && screen.fg_depth[index] <= depth {
            screen.fg_depth[index] = depth;
            let glyphs = screen.glyphs_mut();
            glyphs[index].foreground = fore.unwrap();
        }

        if back.is_some() && screen.bg_depth[index] <= depth {
            screen.bg_depth[index] = depth;
            let glyphs = screen.glyphs_mut();
            glyphs[index].background = back.unwrap();
        }
    }
}

pub fn paint_all_tiles(fore: Option<Color>, back: Option<Color>) {
    let screen = &mut ENGINE_STATE.lock().unwrap().render_state.screen;
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
