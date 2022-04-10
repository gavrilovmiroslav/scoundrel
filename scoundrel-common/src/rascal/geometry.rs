use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use bresenham::Bresenham;
use crate::rascal::interpreter::{Geom, RascalValue};
use crate::rascal::parser::GeometryOp;
use crate::rascal::world::{SetId, World};

pub type PointSet = HashSet<(i32, i32)>;

fn evaluate_rect(x1: i32, y1: i32, x2: i32, y2: i32) -> PointSet {
    let mut set = HashSet::new();
    for i in min(x1, x2)..max(x1, x2) {
        for j in min(y1, y2)..max(y1, y2) {
            set.insert((i, j));
        }
    }

    return set
}

fn evaluate_circle(x: i32, y: i32, r: i32) -> PointSet {
    let mut set = HashSet::new();

    for i in -r..r + 1 {
        for j in -r..r + 1 {
            if i * i + j * j <= r * r {
                set.insert((x + i, y + j));
            }
        }
    }

    return set
}

fn evaluate_line(x1: i32, y1: i32, x2: i32, y2: i32) -> PointSet {
    let mut set = HashSet::new();
    for p in Bresenham::new(
        bresenham::Point::from((x1 as isize, y1 as isize)),
        bresenham::Point::from((x2 as isize, y2 as isize))) {
        set.insert((p.0 as i32, p.1 as i32));
    }

    return set
}

pub fn evaluate_geometry(g: Geom, world: &mut World) -> PointSet {
    if world.set_cache.contains_key(&g) {
        return world.set_cache.get(&g).unwrap().clone();
    }

    let v = match g.clone() {
        Geom::Empty => HashSet::new(),
        Geom::Rect(x1, y1, x2, y2) => evaluate_rect(x1, y1, x2, y2),
        Geom::Circle(x, y, r) => evaluate_circle(x, y, r),
        Geom::Line(x1, y1, x2, y2) => evaluate_line(x1, y1, x2, y2),
        Geom::Bool(op, a, b) => {
            let set_a = evaluate_geometry(a.as_ref().clone(), world);
            let set_b = evaluate_geometry(b.as_ref().clone(), world);
            match op {
                GeometryOp::Un => set_a.union(&set_b).collect::<HashSet<_>>(),
                GeometryOp::Int => set_a.intersection(&set_b).collect::<HashSet<_>>(),
                GeometryOp::Diff => set_a.difference(&set_b).collect::<HashSet<_>>(),
            }.iter().map(|&(i, j)| { (*i, *j) }).collect::<HashSet<_>>()
        }

        Geom::Shrink(g, n) => {
            let geom = if let Geom::Rect(x1, y1, x2, y2) = g.as_ref() {
                Geom::Rect(x1 + 1, y1 + 1, x2 - 1, y2 - 1)
            } else {
                g.as_ref().clone()
            };

            evaluate_geometry(geom, world)
        }

        Geom::Expand(g, n) => {
            let geom = if let Geom::Rect(x1, y1, x2, y2) = g.as_ref() {
                Geom::Rect(x1 - 1, y1 - 1, x2 + 1, y2 + 1)
            } else {
                g.as_ref().clone()
            };

            evaluate_geometry(geom, world)
        }
    };

    world.set_cache.insert(g, v.clone());
    return v
}
