use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::collections::hash_set::Iter;
use std::hash::{Hash, Hasher};
use std::ops::{BitAndAssign,};
use lazy_static::lazy_static;
use std::sync::Mutex;
use bitmaps::Bitmap;
use serde::Serialize;
use serde::Deserialize;
use sparseset::SparseSet;

lazy_static! {
    pub static ref WORLD: Mutex<World> = Mutex::new(World::default());
    pub static ref TYPES: Mutex<HashMap<TypeId, String>> = Mutex::new(HashMap::default());
}

pub fn get_typename(tid: TypeId) -> String {
    TYPES.lock().unwrap().get(&tid).unwrap().to_string()
}

pub type WorldBitmap = Bitmap<{ World::STORAGE_SIZE }>;
pub type TypeId = u64;

#[derive(Clone)]
#[derive(Eq, Hash, PartialEq, Ord, PartialOrd, Debug)]
#[derive(Serialize, Deserialize)]
pub struct Entity(pub usize);
impl Copy for Entity {}

#[derive(Default)]
pub struct OneToMany<T, U>(pub HashMap<T, HashSet<U>>);

impl<T: Eq + Hash + Copy, U: Eq + Hash> OneToMany<T, U> {
    pub fn contains(&mut self, key: &T) -> bool {
        self.0.contains_key(key)
    }

    pub fn insert(&mut self, key: T, value: U) {
        if !self.0.contains_key(&key) {
            self.0.insert(key, HashSet::default());
        }

        self.0.get_mut(&key).unwrap().insert(value);
    }

    pub fn get(&self, key: &T) -> Option<Iter<'_, U>> {
        if let Some(it) = self.0.get(key) {
            Some(it.iter())
        } else {
            None
        }
    }
}

pub struct World {
    pub last_entity: usize,
    pub components: HashMap<TypeId, SparseSet<Vec<u8>>>,
    pub entities: HashSet<Entity>,
    pub component_index: HashMap<TypeId, HashSet<Entity>>,
    pub query_cache: HashMap<TypeId, Vec<u8>>,
    pub type_dependencies: OneToMany<TypeId, TypeId>,
}

impl World {
    pub(crate) const STORAGE_SIZE: usize = 512;
}

impl Default for World {
    fn default() -> Self {
        World {
            last_entity: 0,
            components: HashMap::default(),
            entities: HashSet::default(),
            component_index: HashMap::default(),
            query_cache: HashMap::default(),
            type_dependencies: OneToMany::default(),
        }
    }
}

pub fn generate_typeid<T>() -> TypeId {
    let typename = std::any::type_name::<T>();
    let mut hasher = DefaultHasher::new();
    hasher.write(typename.as_bytes());
    let hash = hasher.finish();
    let mut types = TYPES.lock().unwrap();
    if !types.contains_key(&hash) {
        types.insert(hash, typename.to_string());
    }
    hash
}

pub fn create_storage<T: Serialize>(world: &mut World) {
    let tid = generate_typeid::<T>();
    if !world.components.contains_key(&tid) {
        println!("Adding component storage for type {} ({})", std::any::type_name::<T>(), tid);
        world.components.insert(tid, SparseSet::with_capacity(World::STORAGE_SIZE));
    }

    if !world.component_index.contains_key(&tid) {
        world.component_index.insert(tid, HashSet::default());
    }
}

pub fn copy_into_storage<T: Serialize>(world: &mut World, t: &T, e: Entity) {
    assert!(e.0 < World::STORAGE_SIZE);
    let bin_vec = bincode::serialize(&t).unwrap();
    let tid = generate_typeid::<T>();

    if let Some(comp) = world.component_index.get_mut(&tid) {
        comp.insert(e);
    }

    if let Some(comp) = world.components.get_mut(&tid) {
        comp.insert(e.0, bin_vec);
    }
}

pub fn get_existing_indices<T>(world: &World, bitmap: &mut WorldBitmap) {
    let tid = generate_typeid::<T>();

    if let Some(indices) = world.component_index.get(&tid) {
        let mut other = WorldBitmap::default();
        for e in indices {
            other.set(e.0, true);
        }
        bitmap.bitand_assign(other);
    }
}

pub fn get_cached_query_result<T>(world: &World) -> Option<Vec<(Entity, T)>>
    where T: for<'de> Deserialize<'de> {

    let tid = generate_typeid::<T>();
    if let Some(cached) = world.query_cache.get(&tid) {
        let data = bincode::deserialize::<Vec<(Entity, T)>>(&cached[..]);

        match data {
            Ok(result) =>
                Some(result),
            Err(e) => {
                println!("{:?}", e.to_string());
                None
            }
        }
    } else {
        None
    }
}

pub fn cache_query_result<T>(world: &mut World, query_result: &Vec<(Entity, T)>)
    where T: Serialize {
    let tid = generate_typeid::<T>();
    let bin_vec = bincode::serialize(query_result).unwrap();
    world.query_cache.insert(tid, bin_vec);
}

pub fn clear_query_result_cache_for_typeid(world: &mut World, tid: TypeId) {
    world.query_cache.remove(&tid);
    if let Some(its) = world.type_dependencies.get(&tid) {
        for id in its {
            world.query_cache.remove(id);
        }
    }
}

pub fn clear_query_result_cache<T>(world: &mut World) {
    let tid = generate_typeid::<T>();
    clear_query_result_cache_for_typeid(world, tid);
}

pub fn read_from_storage<T: for<'de> Deserialize<'de>>(world: &World, e: Entity) -> Option<T> {
    assert!(e.0 < World::STORAGE_SIZE);

    let tid = generate_typeid::<T>();
    let typename = std::any::type_name::<T>();

    if !world.components.contains_key(&tid) {
        println!("Failed to get component for type {} ({})", typename, tid);
        return None;
    }

    if !world.component_index.get(&tid).unwrap().contains(&e) {
        println!("Failed to get component at index {} for type {}", e.0, typename);
        return None;
    }

    let data = {
        let bin_data = world.components.get(&tid).unwrap().get(e.0).cloned().unwrap();
        bincode::deserialize::<T>(&bin_data[..])
    };

    match data {
        Ok(result) =>
            Some(result),
        Err(e) => {
            println!("{:?}", e.to_string());
            None
        }
    }
}
