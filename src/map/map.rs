use crate::engine::ENGINE_STATE;
use crate::map::{center};
use crate::map::{intersection_point, walls};
use crate::map::{line, StencilImpl};
use crate::map::{BrushSetter, Field};
use crate::{Color, distance, Glyph, Point, print_char_colors, Renderable};
use std::collections::HashMap;
use std::collections::HashSet;
use std::default::Default;
use crate::map::fov;
use crate::paint_tile;
use crate::print_glyph;
use serde::*;
use crate::gameplay::FieldOfView;


pub struct FOVDescriptor {
    pub minimal_visible_color_value: u8,
    pub render_seen_history: bool,
    pub use_distance_falloff: bool,
}

impl Default for FOVDescriptor {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl FOVDescriptor {
    pub const SIMPLE: FOVDescriptor = Self {
        minimal_visible_color_value: 0,
        render_seen_history: false,
        use_distance_falloff: false,
    };

    pub const DEFAULT: FOVDescriptor = Self {
        minimal_visible_color_value: 40,
        render_seen_history: true,
        use_distance_falloff: true,
    };

    pub const SIMPLE_MEMORY: FOVDescriptor = Self {
        minimal_visible_color_value: 40,
        render_seen_history: true,
        use_distance_falloff: false,
    };
}

#[derive(Serialize, Deserialize, Clone)]
pub struct MapLevel {
    pub size: (u32, u32),
    pub walkable: Field<bool>,
    pub seen: Field<Glyph>,
    pub drawn: Field<Glyph>,
    pub rooms: HashMap<Point, StencilImpl>,
    pub centers: Vec<Point>,
    pub connected: HashSet<(Point, Point)>,
}



impl Default for MapLevel {
    fn default() -> MapLevel {
        let screen_size = ENGINE_STATE.lock().unwrap().render_state.screen_size;
        MapLevel::new(screen_size)
    }
}

impl MapLevel {
    pub fn new(size: (u32, u32)) -> MapLevel {
        MapLevel {
            size,
            walkable: Default::default(),
            seen: Default::default(),
            drawn: Default::default(),
            rooms: Default::default(),
            centers: Default::default(),
            connected: Default::default(),
        }
    }

    pub fn mark_seen(&mut self, fov: &FieldOfView) {
        for (pt, _) in &fov.0 {
            self.seen.set(pt, *self.drawn.values.get(pt).unwrap_or(&Glyph::WALL));
        }
    }

    pub fn fov(&mut self, pt: &Point, distance: u16) -> FieldOfView {
        let f = fov(*pt, distance, &self.walkable);
        self.mark_seen(&f);
        f
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

pub struct FOVRenderingPass<'a> {
    pub map: &'a MapLevel,
    pub origin: &'a Point,
    pub fov_descriptor: &'a FOVDescriptor,
    pub fov: &'a FieldOfView,
    pub max_distance: u16,
}

pub struct FOVRenderer<'a> {
    pub render_seen: &'a dyn Fn(Point, Glyph),
    pub render_fov: &'a dyn Fn(Point, bool),
}

impl<'a> Renderable for (FOVRenderingPass<'a>, FOVRenderer<'a>) {
    fn render(&self) {
        if self.0.fov_descriptor.render_seen_history {
            for (p, g) in &self.0.map.seen.values {
                (self.1.render_seen)(*p, *g);
            }
        }

        for (p, state) in &self.0.fov.0 {
            (self.1.render_fov)(*p, *state);
        }
    }
}

impl<'a> Renderable for FOVRenderingPass<'a> {
    fn render(&self) {
        if self.fov_descriptor.render_seen_history {
            for (p, g) in &self.map.seen.values {
                print_char_colors(*p, g.symbol.unwrap(),
                                  Color::new(0, 0, self.fov_descriptor.minimal_visible_color_value),
                                  Color::BLACK, 4);
            }
        }

        for (p, state) in &self.fov.0 {
            let glyph = if !state { Glyph::WALL } else { Glyph::FLOOR };

            let val = if self.fov_descriptor.use_distance_falloff {
                let m = self.max_distance as f32;
                let d = distance(*self.origin, *p).clamp(0.0, m);
                let f = 255 - (d / m * 255.0) as u8;
                f.max(self.fov_descriptor.minimal_visible_color_value)
            } else {
                255
            };

            print_glyph(*p, glyph, 5);
            paint_tile(*p, Some(Color::new(0, 0, val)), None, 5);
        }
    }
}