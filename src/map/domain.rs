use crate::map::types::*;
use crate::map::{Rasterize, Stencil};
use crate::{paint_tile, print_char, Color, Glyph, GlyphTint, Point, Renderable, BLACK, GRAY};
use multimap::MultiMap;
use std::collections::HashMap;

#[derive(Default)]
pub struct Domain<T: Clone + Copy> {
    pub values: MultiMap<Point, T>,
}

impl<T: Clone + Copy> Domain<T> {
    pub fn new() -> Self {
        Domain {
            values: MultiMap::default(),
        }
    }

    pub fn contains(&self, p: &Point) -> bool {
        self.values.contains_key(p)
    }
}

impl<T: Clone + Copy> BrushSetter<Point, T> for Domain<T> {
    fn set(&mut self, tile: &Point, value: T) {
        self.values.insert(tile.clone(), value);
    }
}

impl<T: Clone + Copy> BrushSetter<Box<Stencil>, T> for Domain<T> {
    fn set(&mut self, brush: &Box<Stencil>, value: T) {
        for tile in brush.rasterize(Point::from((0, 0))) {
            self.values.insert(tile, value);
        }
    }
}

impl<T: Clone + Copy> BrushSetter<Stencil, T> for Domain<T> {
    fn set(&mut self, brush: &Stencil, value: T) {
        for tile in brush.rasterize(Point::from((0, 0))) {
            self.values.insert(tile, value);
        }
    }
}

impl<P: Into<Point> + Clone, T: Clone + Copy> BrushGetter<P, usize> for Domain<T> {
    fn get(&self, p: P) -> usize {
        self.values.get_vec(&p.into()).unwrap_or(&Vec::new()).len()
    }
}

impl<P: Into<Point> + Clone, T: Clone + Copy> BrushAllGetter<P, T> for Domain<T> {
    fn get(&self, p: P) -> Vec<T> {
        self.values
            .get_vec(&p.into())
            .unwrap_or(&Vec::new())
            .clone()
    }
}

impl<T: Clone + Copy> BrushOverlaps<Stencil> for Domain<T> {
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

impl<T: Clone + Copy> BrushOverlapsOffset<Stencil> for Domain<T> {
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

impl Renderable for Domain<Glyph> {
    fn render(&self, origin: Point) {
        for (k, v) in &self.values {
            // TODO:
        }
    }
}

impl Renderable for Domain<i8> {
    fn render(&self, origin: Point) {
        for (k, v) in &self.values {
            let p = *k + origin;
            // TODO:
        }
    }
}

impl Renderable for Domain<u8> {
    fn render(&self, origin: Point) {
        for (k, v) in &self.values {
            let p = *k + origin;
            // TODO:
        }
    }
}
