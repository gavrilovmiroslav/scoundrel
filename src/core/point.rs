use std::ops::{Add, AddAssign, Sub, SubAssign};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Point {
    pub x: i16,
    pub y: i16,
}

impl From<(i32, i32)> for Point {
    fn from(pt: (i32, i32)) -> Self {
        Point {
            x: pt.0 as i16,
            y: pt.1 as i16,
        }
    }
}

impl From<(i16, i16)> for Point {
    fn from(pt: (i16, i16)) -> Self {
        Point { x: pt.0, y: pt.1 }
    }
}

impl From<(u32, u32)> for Point {
    fn from(pt: (u32, u32)) -> Self {
        Point {
            x: pt.0 as i16,
            y: pt.1 as i16,
        }
    }
}

impl From<Point> for (u32, u32) {
    fn from(p: Point) -> Self {
        (p.x as u32, p.y as u32)
    }
}

impl From<Point> for (i16, i16) {
    fn from(p: Point) -> Self {
        (p.x, p.y)
    }
}

impl From<bresenham::Point> for Point {
    fn from(p: bresenham::Point) -> Self {
        Point {
            x: p.0 as i16,
            y: p.1 as i16,
        }
    }
}

impl Into<bresenham::Point> for Point {
    fn into(self) -> bresenham::Point {
        (self.x as isize, self.y as isize)
    }
}

pub fn distance(xy: Point, pq: Point) -> f32 {
    let dx = xy.x as f32 - pq.x as f32;
    let dy = xy.y as f32 - pq.y as f32;
    (dx * dx + dy * dy).sqrt()
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl Sub for Point {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
        }
    }
}

impl SubAssign for Point {
    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}
