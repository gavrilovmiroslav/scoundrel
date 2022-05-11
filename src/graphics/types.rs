use crate::Point;

pub trait Renderable {
    fn render(&self, origin: Point);
}
