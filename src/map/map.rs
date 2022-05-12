use crate::engine::ENGINE_STATE;
use crate::map::center;
use crate::map::StencilImpl;
use crate::map::{BrushSetter, Field};
use crate::{Glyph, Point};
use petgraph::Graph;
use std::collections::HashMap;
use std::collections::HashSet;

pub struct MapLevelComponent {
    pub size: (u32, u32),
    pub walkable: Field<bool>,
    pub drawn: Field<Glyph>,
    pub rooms: HashMap<Point, StencilImpl>,
    pub connected: HashSet<(Point, Point)>,
}

impl Default for MapLevelComponent {
    fn default() -> MapLevelComponent {
        let screen_size = ENGINE_STATE.lock().unwrap().render_state.screen_size;
        MapLevelComponent::new(screen_size)
    }
}

impl MapLevelComponent {
    pub fn new(size: (u32, u32)) -> MapLevelComponent {
        MapLevelComponent {
            size,
            walkable: Default::default(),
            drawn: Default::default(),
            rooms: Default::default(),
            connected: Default::default(),
        }
    }

    pub fn put_walkable_space(&mut self, stencil: &StencilImpl) {
        self.walkable.set(stencil, true);
    }

    pub fn put_visible_space(&mut self, stencil: &StencilImpl, glyph: Glyph) {
        self.drawn.set(stencil, glyph);
    }

    pub fn put_space(&mut self, stencil: &StencilImpl, glyph: Glyph) {
        self.put_walkable_space(stencil);
        self.put_visible_space(stencil, glyph);
    }

    pub fn put_room(&mut self, stencil: &StencilImpl, glyph: Glyph) {
        self.put_space(stencil, glyph);
        self.rooms.insert(center(stencil), stencil.clone());
    }

    pub fn connect(&mut self, a: Point, b: Point) {
        self.connected.insert((a, b));
        self.connected.insert((b, a));
    }

    pub fn is_connected(&mut self, a: Point, b: Point) -> bool {
        self.connected.contains(&(a, b)) || self.connected.contains(&(b, a))
    }
}
