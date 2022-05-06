use crate::map::{Rasterize, Stencil};
use crate::Point;
use std::collections::HashMap;

pub trait FieldSetter<Brush, T> {
    fn set(&mut self, brush: &Brush, value: T);
}

pub trait FieldMaybeGetter<P, T> {
    fn maybe_get(&self, p: P) -> Option<T>;
}

pub trait FieldGetter<P, T> {
    fn get(&self, p: P) -> T;
}

pub struct Field<T: Clone + Copy> {
    values: HashMap<Point, T>,
}

impl<T: Clone + Copy> Field<T> {
    pub fn new() -> Self {
        Field {
            values: HashMap::default(),
        }
    }
}

impl<T: Clone + Copy> FieldSetter<Stencil, T> for Field<T> {
    fn set(&mut self, brush: &Stencil, value: T) {
        for tile in brush.rasterize(Point::from((0, 0))) {
            self.values.insert(tile, value);
        }
    }
}

impl<P: Into<Point> + Clone, T: Clone + Copy> FieldMaybeGetter<P, T> for Field<T> {
    fn maybe_get(&self, p: P) -> Option<T> {
        let res = self.values.get(&p.into());
        res.map(T::clone)
    }
}

impl<P: Into<Point> + Clone, T: Clone + Copy + Default> FieldGetter<P, T> for Field<T> {
    fn get(&self, p: P) -> T {
        let res = self.values.get(&p.into());
        res.map(T::clone).unwrap_or_default()
    }
}
