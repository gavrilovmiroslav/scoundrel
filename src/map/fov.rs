use std::collections::HashMap;
use crate::map::{BrushGetter, Field};
use crate::map::shadow_casting::Shadowcaster;
use crate::Point;
use crate::map::shadow_casting::SymmetricShadowcast;
use std::sync::Mutex;
use lazy_static::*;
use crate::gameplay::FieldOfView;

lazy_static! {
    pub static ref FOV_CACHE: Mutex<HashMap<Point, FieldOfView>> =
        Mutex::new(HashMap::default());
}

pub fn general_fov<S: Shadowcaster>(origin: Point, max_depth: u16, field: &Field<bool>) -> HashMap<Point, bool> {
    let mut mark = HashMap::new();
    S::compute(
        origin,
        max_depth,
        &|pt| { !field.get(pt) },
        &mut mark);
    mark
}

pub fn uncache_fov(origin: &Point) {
    let mut cache = FOV_CACHE.lock().unwrap();
    cache.remove(origin);
}

pub fn fov(origin: Point, max_depth: u16, field: &Field<bool>) -> FieldOfView {
    let mut cache = FOV_CACHE.lock().unwrap();

    if !cache.contains_key(&origin) {
        let fov = general_fov::<SymmetricShadowcast>(origin, max_depth, field)
            .into_iter()
            .filter(|(p, _)| p.is_non_negative())
            .map(|(p, _)| (p, field.contains(&p)))
            .collect::<HashMap<_, _>>();

        cache.insert(origin, FieldOfView(fov.clone()));
    }

    cache.get(&origin).unwrap().clone()
}