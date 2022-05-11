use crate::{print_char, Point};
use bresenham::Bresenham;
use std::collections::HashSet;

pub type RasterIter = Vec<Point>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Bool {
    Union,
    Diff,
    Intersect,
}

pub trait Rasterize {
    fn rasterize(&self, origin: Point) -> RasterIter;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Stencil {
    Empty,
    Line {
        a: Point,
        b: Point,
    },
    Rectangle {
        xy: Point,
        w: u16,
        h: u16,
    },
    Circle {
        center: Point,
        radius: u16,
    },
    Boolean {
        op: Bool,
        lhs: Box<Stencil>,
        rhs: Box<Stencil>,
    },
    Resize {
        amount: i16,
        target: Box<Stencil>,
    },
}

pub type StencilImpl = Box<Stencil>;

pub fn empty() -> StencilImpl {
    Box::new(Stencil::Empty)
}

pub fn line<P: Into<Point>, Q: Into<Point>>(p: P, q: Q) -> StencilImpl {
    Box::new(Stencil::Line {
        a: p.into(),
        b: q.into(),
    })
}

pub fn rect<P: Into<Point>>(xy: P, w: u16, h: u16) -> StencilImpl {
    Box::new(Stencil::Rectangle {
        xy: xy.into(),
        w,
        h,
    })
}

pub fn circle<P: Into<Point>>(c: P, r: u16) -> StencilImpl {
    Box::new(Stencil::Circle {
        center: c.into(),
        radius: r,
    })
}

pub fn center(s: &StencilImpl) -> Point {
    match s.as_ref() {
        &Stencil::Rectangle { xy, w, h } => xy + (w / 2, h / 2).into(),
        &Stencil::Circle { center, .. } => center,
        &Stencil::Line { a, b } => (a + b) / 2,
        Stencil::Resize { target, .. } => center(target),
        Stencil::Boolean { lhs, rhs, .. } => (center(lhs) + center(rhs)) / 2,
        _ => Point::from((0, 0)),
    }
}

pub fn union(a: &StencilImpl, b: &StencilImpl) -> StencilImpl {
    Box::new(Stencil::Boolean {
        op: Bool::Union,
        lhs: Box::clone(a),
        rhs: Box::clone(b),
    })
}

pub fn diff(a: &StencilImpl, b: &StencilImpl) -> StencilImpl {
    Box::new(Stencil::Boolean {
        op: Bool::Diff,
        lhs: Box::clone(a),
        rhs: Box::clone(b),
    })
}

pub fn intersect(a: &StencilImpl, b: &StencilImpl) -> StencilImpl {
    Box::new(Stencil::Boolean {
        op: Bool::Intersect,
        lhs: Box::clone(a),
        rhs: Box::clone(b),
    })
}

pub fn grow(t: &StencilImpl, n: u16) -> StencilImpl {
    Box::new(Stencil::Resize {
        amount: n as i16,
        target: Box::clone(t),
    })
}

pub fn shrink(t: &StencilImpl, n: u16) -> StencilImpl {
    Box::new(Stencil::Resize {
        amount: -(n as i16),
        target: Box::clone(t),
    })
}

pub fn walls(room: &StencilImpl) -> StencilImpl {
    let non_walls = shrink(room, 1);
    diff(room, &non_walls)
}

pub fn dup(t: &StencilImpl) -> StencilImpl {
    Box::clone(&t)
}

impl Rasterize for Stencil {
    fn rasterize(&self, origin: Point) -> RasterIter {
        match self {
            Stencil::Empty => vec![],

            Stencil::Line { a, b } => Bresenham::new(a.clone().into(), b.clone().into())
                .map(Point::from)
                .collect(),

            Stencil::Rectangle { xy, w, h } => {
                let mut vec = Vec::with_capacity((w * h) as usize);
                for x in 0..*w {
                    for y in 0..*h {
                        vec.push(Point::from((
                            (xy.x + x as i16 + origin.x) as i16,
                            (xy.y + y as i16 + origin.y) as i16,
                        )));
                    }
                }
                vec
            }

            Stencil::Circle { radius, center } => {
                let mut vec = Vec::with_capacity((radius * radius) as usize);
                let r = *radius as i16;
                for x in -r..r {
                    for y in -r..r {
                        if x * x + y * y <= r * r {
                            vec.push(Point::from((
                                (center.x + x + origin.x) as i16,
                                (center.y + y + origin.y) as i16,
                            )));
                        }
                    }
                }
                vec
            }

            Stencil::Boolean {
                op: Bool::Union,
                lhs,
                rhs,
            } => {
                let ls: HashSet<_> = HashSet::from_iter(lhs.rasterize(origin));
                let rs: HashSet<_> = HashSet::from_iter(rhs.rasterize(origin));
                ls.union(&rs).map(Point::clone).collect()
            }

            Stencil::Boolean {
                op: Bool::Diff,
                lhs,
                rhs,
            } => {
                let ls: HashSet<_> = HashSet::from_iter(lhs.rasterize(origin));
                let rs: HashSet<_> = HashSet::from_iter(rhs.rasterize(origin));
                ls.difference(&rs).map(Point::clone).collect()
            }

            Stencil::Boolean {
                op: Bool::Intersect,
                lhs,
                rhs,
            } => {
                let ls: HashSet<_> = HashSet::from_iter(lhs.rasterize(origin));
                let rs: HashSet<_> = HashSet::from_iter(rhs.rasterize(origin));
                ls.intersection(&rs).map(Point::clone).collect()
            }

            Stencil::Resize { target, amount } => {
                let changed = match target.as_ref() {
                    Stencil::Rectangle { xy, w, h } => {
                        let bad = if *amount < 0 {
                            let norm = amount.abs() as u16;
                            *w <= norm || *h <= norm
                        } else {
                            false
                        };

                        if bad {
                            None
                        } else {
                            Some(Stencil::Rectangle {
                                xy: Point::from((xy.x - amount, xy.y - amount)),
                                w: (*w as i16 + 2 * *amount) as u16,
                                h: (*h as i16 + 2 * *amount) as u16,
                            })
                        }
                    }
                    Stencil::Circle { center, radius } => {
                        let bad = if *amount < 0 {
                            let norm = amount.abs() as u16;
                            *radius <= norm
                        } else {
                            false
                        };

                        if bad {
                            None
                        } else {
                            Some(Stencil::Circle {
                                center: *center,
                                radius: (*radius as i16 + *amount) as u16,
                            })
                        }
                    }
                    _ => None,
                };

                if changed.is_some() {
                    changed.unwrap().rasterize(origin)
                } else {
                    vec![]
                }
            }
        }
    }
}

pub fn paint_stencil(stencil: &StencilImpl, sym: char, depth: u32) {
    for tile in stencil.rasterize(Point::from((0, 0))) {
        print_char(tile, sym, depth);
    }
}

pub fn spread_fill<T, P: Fn(&Point) -> bool, F: FnMut(&Point) -> T>(start: Point, p: P, mut f: F) {
    let mut done = vec![];

    fn spread_fill_rec<T, P: Fn(&Point) -> bool, F: FnMut(&Point) -> T>(
        pt: &Point,
        p: &P,
        f: &mut F,
        done: &mut Vec<Point>,
    ) {
        if !done.contains(&pt) && p(pt) {
            done.push(pt.clone());
            f(pt);

            for next_pt in pt.neighbors() {
                spread_fill_rec(&next_pt, p, f, done);
            }
        }
    }

    spread_fill_rec(&start, &p, &mut f, &mut done)
}
