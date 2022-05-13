use crate::engine::ENGINE_STATE;
use crate::map::{center, corner};
use crate::map::{intersection_point, walls};
use crate::map::{line, StencilImpl};
use crate::map::{BrushSetter, Field};
use crate::{distance, rand, Glyph, Point};
use petgraph::Graph;
use std::collections::HashMap;
use std::collections::HashSet;
use std::default::Default;

pub struct MapLevelComponent {
    pub size: (u32, u32),
    pub walkable: Field<bool>,
    pub drawn: Field<Glyph>,
    pub rooms: HashMap<Point, StencilImpl>,
    pub centers: Vec<Point>,
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
            centers: Default::default(),
            connected: Default::default(),
        }
    }

    pub fn stamp_walkable(&mut self, stencil: &StencilImpl) {
        self.walkable.set(stencil, true);
    }

    pub fn stamp_visible(&mut self, stencil: &StencilImpl, glyph: Glyph) {
        self.drawn.set(stencil, glyph);
    }

    pub fn stamp_visible_and_walkable(&mut self, stencil: &StencilImpl, glyph: Glyph) {
        self.stamp_walkable(stencil);
        self.stamp_visible(stencil, glyph);
    }

    pub fn put_room(&mut self, stencil: &StencilImpl, glyph: Glyph) {
        self.stamp_visible_and_walkable(stencil, glyph);
        let c = center(stencil);
        self.rooms.insert(c, stencil.clone());
        self.centers.push(c);
    }

    pub fn connect(&mut self, a: Point, b: Point) {
        self.connected.insert((a, b));
        self.connected.insert((b, a));
    }

    pub fn put_corridor(
        &mut self,
        a: Point,
        b: Point,
        stencil_fn: &dyn Fn(Point, Point) -> StencilImpl,
        glyph: Glyph,
    ) {
        let line = stencil_fn(a, b);
        self.stamp_visible_and_walkable(&line, glyph);
        self.connect(a, b);
    }

    pub fn is_connected(&mut self, a: Point, b: Point) -> bool {
        self.connected.contains(&(a, b)) || self.connected.contains(&(b, a))
    }

    pub fn corridor_between(&self, a: Point, b: Point) -> (Point, Point, f32) {
        let full_line = line(a, b);

        // walls
        let wa = walls(self.rooms.get(&a).unwrap());
        let wb = walls(self.rooms.get(&b).unwrap());

        // doors
        let da = intersection_point(&full_line, &wa);
        let db = intersection_point(&wb, &full_line);

        (da, db, distance(da, db))
    }
}
