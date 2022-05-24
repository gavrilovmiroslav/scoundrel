
pub trait BrushSetter<Brush, T> {
    fn set(&mut self, brush: &Brush, value: T);
}

pub trait BrushMaybeGetter<P, T> {
    fn maybe_get(&self, p: P) -> Option<T>;
}

pub trait BrushGetter<P, T> {
    fn get(&self, p: &P) -> T;
}

pub trait BrushAllGetter<P, T> {
    fn get(&self, p: &P) -> Vec<T>;
}

pub trait BrushOverlaps<Brush> {
    fn overlaps(&self, b: &Brush) -> bool;
}
