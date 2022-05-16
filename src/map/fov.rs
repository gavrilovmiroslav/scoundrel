use std::sync::Mutex;
use crate::distance;
use crate::map::{BrushGetter, Field};
use crate::map::shadow_casting::Shadowcaster;
use crate::Point;
use crate::map::shadow_casting::SymmetricShadowcast;

pub fn general_fov<S: Shadowcaster>(origin: Point, max_depth: u16, field: &Field<bool>) -> Vec<(Point, bool)> {
    let mut vec = Mutex::new(Vec::new());
    S::compute(
        origin,
        max_depth,
        &|pt| { !field.get(pt) },
        &|pt, state| {
            vec.lock().unwrap().push((pt.clone(), state))
        });
    let ret = vec.lock().unwrap().to_owned();
    ret
}

pub fn fov(origin: Point, max_depth: u16, field: &Field<bool>) -> Vec<(Point, bool)> {
    general_fov::<SymmetricShadowcast>(origin, max_depth, field)
        .into_iter()
        .filter(|(p, _)| p.is_non_negative())
        .map(|(p, _)| (p, field.contains(&p)))
        .collect::<Vec<_>>()
}