use std::ops::{Add, AddAssign, Div, Sub, SubAssign};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Point {
    pub x: i16,
    pub y: i16,
}

impl Point {
    pub fn is_non_negative(&self) -> bool {
        self.x >= 0 && self.y >= 0
    }

    pub fn is_zero(&self) -> bool {
        self.x == 0 && self.y == 0
    }

    pub fn adjacent(&self) -> [Point; 8] {
        [
            Point::from((self.x - 1, self.y)),
            Point::from((self.x - 1, self.y - 1)),
            Point::from((self.x + 1, self.y)),
            Point::from((self.x - 1, self.y + 1)),
            Point::from((self.x, self.y - 1)),
            Point::from((self.x + 1, self.y - 1)),
            Point::from((self.x, self.y + 1)),
            Point::from((self.x + 1, self.y + 1)),
        ]
    }

    pub fn neighbors(&self) -> [Point; 4] {
        [
            Point::from((self.x - 1, self.y)),
            Point::from((self.x + 1, self.y)),
            Point::from((self.x, self.y - 1)),
            Point::from((self.x, self.y + 1)),
        ]
    }

    pub fn dist(&self, other: &Point) -> f32 {
        let dx = (other.x - self.x) as f32;
        let dy = (other.y - self.y) as f32;
        (dx * dx + dy * dy).sqrt()
    }

    pub fn left(&self) -> Point {
        Point::from((self.x - 1, self.y))
    }

    pub fn right(&self) -> Point {
        Point::from((self.x + 1, self.y))
    }

    pub fn up(&self) -> Point {
        Point::from((self.x, self.y - 1))
    }

    pub fn down(&self) -> Point {
        Point::from((self.x, self.y + 1))
    }
}

impl From<(i32, i32)> for Point {
    fn from(pt: (i32, i32)) -> Self {
        Point {
            x: pt.0 as i16,
            y: pt.1 as i16,
        }
    }
}

impl From<(u16, u16)> for Point {
    fn from(pt: (u16, u16)) -> Self {
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

impl Div<i16> for Point {
    type Output = Self;

    fn div(self, rhs: i16) -> Self::Output {
        Self {
            x: (self.x / rhs) as i16,
            y: (self.y / rhs) as i16,
        }
    }
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
