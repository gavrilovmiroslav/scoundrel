
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
