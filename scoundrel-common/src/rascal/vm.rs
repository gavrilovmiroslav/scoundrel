use std::ops::{BitAnd, Not};

use bitmaps::*;

use crate::rascal::parser::{ComponentCallSite, RascalBlock};
use crate::rascal::world::MAX_ENTRIES_PER_STORAGE;
use crate::rascal::world::World;

pub struct RascalVM {}

impl RascalVM {
    // TODO: these can maybe be cached
    pub fn query_storages(&mut self, world: &World, comps: &Vec<ComponentCallSite>) -> Bitmap<MAX_ENTRIES_PER_STORAGE> {
        let empty_map = Bitmap::<MAX_ENTRIES_PER_STORAGE>::new().not();
        comps.iter().fold(empty_map, |b, c| {
            assert!(c.is_owned);
            let name = c.name.clone();

            let bitmap = world.storage_bitmaps.get(&name).unwrap();
            if c.not {
                bitmap.not().bitand(b)
            } else {
                bitmap.bitand(b)
            }
        })
    }

    pub fn connect_to_vm<T, U>(&mut self, t: T, u: &U) { }
    pub fn interpret_block(&mut self, b: &RascalBlock) { }
}