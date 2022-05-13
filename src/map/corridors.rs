use crate::map::{corner, line, StencilImpl};
use crate::{rand, Point};

pub fn straight_edge(a: Point, b: Point) -> StencilImpl {
    corner(a, b, rand::bool())
}

pub fn direct(a: Point, b: Point) -> StencilImpl {
    line(a, b)
}
