
use crate::colors::Color;

#[derive(Copy, Clone)]
pub struct Surface<T> {
    value: T,
    depth: u8,
}

impl<T> Surface<T> where T: Copy + Send {
    pub fn new(t: T, depth: u8) -> Self {
        Surface { value: t, depth }
    }

    pub fn update(&mut self, other: Self) {
        if self.depth < other.depth {
            self.depth = other.depth;
            self.value = other.value;
        }
    }
}

pub struct LayerGlyph {
    foreground: Surface<Color>,
    background: Surface<Color>,
    symbol: Surface<u16>,
}

pub struct Glyph {
    foreground: Color,
    background: Color,
    symbol: u16,
}

impl LayerGlyph {
    pub fn set_foreground(&mut self, fg: Surface<Color>) {
        self.foreground.update(fg);
    }

    pub fn set_background(&mut self, bg: Surface<Color>) {
        self.background.update(bg);
    }

    pub fn set_symbol(&mut self, ch: Surface<u16>) {
        self.symbol.update(ch);
    }

    pub fn get(&self) -> Glyph {
        Glyph {
            foreground: self.foreground.value,
            background: self.background.value,
            symbol: self.symbol.value,
        }
    }
}
