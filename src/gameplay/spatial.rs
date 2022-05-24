use crate::{Point};
use serde::*;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Positioned(pub Point);

impl Positioned {
    pub fn at(x: i16, y: i16) -> Self {
        Positioned((x, y).into())
    }

    pub fn new(p: impl Into<Point>) -> Self {
        Positioned(p.into())
    }
}

