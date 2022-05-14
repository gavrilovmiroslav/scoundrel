use crate::map::types::*;
use crate::map::{Rasterize, Stencil};
use crate::{paint_tile, print_char, Color, Glyph, GlyphTint, Point, Renderable};
use std::collections::HashMap;

#[derive(Default, Clone)]
pub struct Field<T: Clone + Copy> {
    pub values: HashMap<Point, T>,
}

impl<T: Clone + Copy> Field<T> {
    pub fn new() -> Self {
        Field {
            values: HashMap::default(),
        }
    }

    pub fn contains(&self, p: &Point) -> bool {
        self.values.contains_key(p)
    }
}

impl<T: Clone + Copy> BrushSetter<Point, T> for Field<T> {
    fn set(&mut self, tile: &Point, value: T) {
        self.values.insert(tile.clone(), value);
    }
}

impl<T: Clone + Copy> BrushSetter<Box<Stencil>, T> for Field<T> {
    fn set(&mut self, brush: &Box<Stencil>, value: T) {
        for tile in brush.rasterize(Point::from((0, 0))) {
            self.values.insert(tile, value);
        }
    }
}

impl<T: Clone + Copy> BrushSetter<Stencil, T> for Field<T> {
    fn set(&mut self, brush: &Stencil, value: T) {
        for tile in brush.rasterize(Point::from((0, 0))) {
            self.values.insert(tile, value);
        }
    }
}

impl<P: Into<Point> + Clone, T: Clone + Copy> BrushMaybeGetter<P, T> for Field<T> {
    fn maybe_get(&self, p: P) -> Option<T> {
        let res = self.values.get(&p.into());
        res.map(T::clone)
    }
}

impl<P: Into<Point> + Clone, T: Clone + Copy + Default> BrushGetter<P, T> for Field<T> {
    fn get(&self, p: P) -> T {
        let res = self.values.get(&p.into());
        res.map(T::clone).unwrap_or_default()
    }
}

impl<T: Clone + Copy> BrushOverlaps<Stencil> for Field<T> {
    fn overlaps(&self, b: &Stencil) -> bool {
        match b {
            Stencil::Empty => false,
            other => {
                for p in other.rasterize((0, 0).into()) {
                    if self.values.contains_key(&p) {
                        return true;
                    }
                }

                return false;
            }
        }
    }
}

impl<T: Clone + Copy> BrushOverlapsOffset<Stencil> for Field<T> {
    fn overlaps_offset<P: Into<Point>>(&self, b: &Stencil, offset: P) -> bool {
        match b {
            Stencil::Empty => false,
            other => {
                for p in other.rasterize(offset.into()) {
                    if self.values.contains_key(&p) {
                        return true;
                    }
                }

                return false;
            }
        }
    }
}

impl Renderable for Field<Glyph> {
    fn render(&self, origin: Point) {
        for (k, v) in &self.values {
            if let Some(chr) = v.symbol {
                let colors = match v.tint {
                    GlyphTint::None => (None, None),
                    GlyphTint::Fore(fg) => (Some(fg), None),
                    GlyphTint::Back(bg) => (None, Some(bg)),
                    GlyphTint::BackFore(bg, fg) => (Some(fg), Some(bg)),
                };

                let p = *k + origin;
                if p.is_non_negative() {
                    print_char(p, chr, 1);
                    paint_tile(p, colors.0, colors.1, 1);
                }
            }
        }
    }
}

impl Renderable for Field<i8> {
    fn render(&self, origin: Point) {
        for (k, v) in &self.values {
            let p = *k + origin;
            if p.is_non_negative() {
                print_char(p, ('0' as u8 + v.abs() as u8) as char, 1);
                paint_tile(
                    p,
                    Some(Color::new(
                        if *v < 0 { 128 } else { 0 },
                        u16::max(20 + 23 * v.abs() as u16, 255) as u8,
                        255,
                    )),
                    Some(Color::BLACK),
                    1,
                );
            }
        }
    }
}

impl Renderable for Field<u8> {
    fn render(&self, origin: Point) {
        for (k, v) in &self.values {
            let p = *k + origin;
            if p.is_non_negative() {
                print_char(p, ('0' as u8 + *v) as char, 1);
                paint_tile(
                    p,
                    Some(Color::new(0, 20 + 23 * *v, 255)),
                    Some(Color::BLACK),
                    1,
                );
            }
        }
    }
}
