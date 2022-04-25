#[derive(Debug)]
pub struct Point {
    pub x: i32,
    pub y: i32,
}

impl From<(i32, i32)> for Point {
    fn from(pt: (i32, i32)) -> Self {
        Point { x: pt.0, y: pt.1 }
    }
}

pub fn distance(xy: Point, pq: Point) -> f32 {
    let dx = xy.x as f32 - pq.x as f32;
    let dy = xy.y as f32 - pq.y as f32;
    (dx * dx + dy * dy).sqrt()
}
