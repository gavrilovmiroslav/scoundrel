use std::collections::HashMap;
use crate::{Glyph, Point};
use crate::gameplay::{Positioned, Visible};
use crate::world::*;
use serde::*;

#[derive(Serialize, Deserialize, Clone, Default)]
pub struct EntityCollection(pub HashMap<Point, Entity>);

impl EntityCollection {
    pub fn insert(&mut self, pt: &Point, glyph: Glyph) -> Entity {
        let entity = World::spawn((
            Positioned(pt.clone()),
            Visible(glyph),));
        self.0.insert(pt.clone(), entity.clone());
        entity
    }

    pub fn remove_at(&mut self, pt: &Point) {
        let door_id = self.0.get(pt).unwrap();
        door_id.despawn();
        self.0.remove(pt);
    }

    pub fn exists_at(&self, pt: &Point) -> bool {
        self.0.contains_key(pt)
    }
}
