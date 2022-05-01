#[derive(Debug, Clone, Copy)]
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

pub fn distance(xy: Point, pq: Point) -> f32 {
    let dx = xy.x as f32 - pq.x as f32;
    let dy = xy.y as f32 - pq.y as f32;
    (dx * dx + dy * dy).sqrt()
}
