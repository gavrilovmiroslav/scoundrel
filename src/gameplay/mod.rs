use crate::{Glyph, GlyphTint, Point};

#[derive(Debug)]
pub struct Positioned(pub Point);

impl Into<Point> for Positioned {
    fn into(self) -> Point {
        self.0
    }
}

impl Into<Positioned> for Point {
    fn into(self) -> Positioned {
        Positioned(self)
    }
}

impl Positioned {
    pub fn at(x: i16, y: i16) -> Self {
        Positioned((x, y).into())
    }

    pub fn new(p: impl Into<Point>) -> Self {
        Positioned(p.into())
    }

    pub fn update(&mut self, p: impl Into<Point>) {
        self.0 = p.into();
    }

    pub fn get(&self) -> Point {
        self.0
    }
}

pub struct Visible(pub Glyph);

impl Visible {
    pub fn new(g: Glyph) -> Self {
        Visible(g)
    }

    pub fn get(&self) -> Glyph {
        self.0
    }
}

impl Into<Glyph> for Visible {
    fn into(self) -> Glyph {
        self.0
    }
}

impl Into<Option<char>> for Visible {
    fn into(self) -> Option<char> {
        self.0.symbol
    }
}

impl Into<GlyphTint> for Visible {
    fn into(self) -> GlyphTint {
        self.0.tint
    }
}

pub struct PlayerTag;
