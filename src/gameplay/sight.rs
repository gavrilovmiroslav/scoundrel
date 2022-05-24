use std::collections::HashMap;
use crate::{Glyph, Point};
use serde::*;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Visible(pub Glyph);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Sighted(pub u16);

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct FieldOfView(pub HashMap<Point, bool>);

impl FieldOfView {
    pub fn empty() -> FieldOfView {
        FieldOfView(HashMap::new())
    }

    pub fn sees(&self, pt: &Point) -> bool {
        self.0.contains_key(pt)
    }
}