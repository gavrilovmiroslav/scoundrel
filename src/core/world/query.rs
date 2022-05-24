use bitmaps::Bitmap;
use serde::{Deserialize, Serialize};
use crate::world::*;

pub trait QueryBundle: Sized {
    fn query() -> Vec<(Entity, Self)>;

    fn query_components() -> Vec<Self> {
        Self::query().into_iter().map(|(_, c)| c).collect()
    }
}

impl World {
    pub fn query<BundledComponents: QueryBundle>() -> Vec<(Entity, BundledComponents)> {
        BundledComponents::query()
    }
}

impl<'a, T> QueryBundle for (T,)
    where T: Serialize + for<'de> Deserialize<'de>
{
    fn query() -> Vec<(Entity, (T,))> {
        let mut world = WORLD.lock().unwrap();
        let mut result = Vec::new();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res
        } else {
            let tid = generate_typeid::<T>();
            world.type_dependencies.insert(tid, tid);

            for e in world.components.get(&tid).unwrap() {
                let index = Entity(e.key());
                result.push((index,
                             (read_from_storage::<T>(&world, index)
                                  .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T>()).as_str()), ),
                ));
            }

            cache_query_result(&mut world, &result);
            result
        }
    }
}

impl<'a, T1, T2> QueryBundle for (T1, T2)
    where T1: Serialize + for<'de> Deserialize<'de>,
          T2: Serialize + for<'de> Deserialize<'de>,
{
    fn query() -> Vec<(Entity, Self)> {
        let mut world = WORLD.lock().unwrap();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res
        } else {
            let tid = generate_typeid::<(T1, T2)>();
            if !world.type_dependencies.contains(&tid) {
                world.type_dependencies.insert(generate_typeid::<T1>(), tid);
                world.type_dependencies.insert(generate_typeid::<T2>(), tid);
            }

            let mut result = Vec::new();

            let indexed_entities: WorldBitmap = {
                let mut bits = WorldBitmap::new();
                bits.invert();
                get_existing_indices::<T1>(&world, &mut bits);
                get_existing_indices::<T2>(&world, &mut bits);
                bits
            };

            for index in &world.entities {
                if indexed_entities.get(index.0) {
                    result.push((*index, (
                        read_from_storage::<T1>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T1>()).as_str()),
                        read_from_storage::<T2>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T2>()).as_str()),
                    )));
                }
            }

            cache_query_result(&mut world, &result);
            result
        }
    }
}

impl<'a, T1, T2, T3> QueryBundle for (T1, T2, T3)
    where T1: Serialize + for<'de> Deserialize<'de>,
          T2: Serialize + for<'de> Deserialize<'de>,
          T3: Serialize + for<'de> Deserialize<'de>,
{
    fn query() -> Vec<(Entity, Self)> {
        let mut world = WORLD.lock().unwrap();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res
        } else {
            let tid = generate_typeid::<(T1, T2, T3)>();
            if !world.type_dependencies.contains(&tid) {
                world.type_dependencies.insert(generate_typeid::<T1>(), tid);
                world.type_dependencies.insert(generate_typeid::<T2>(), tid);
                world.type_dependencies.insert(generate_typeid::<T3>(), tid);
            }

            let mut result = Vec::new();
            let indexed_entities = {
                let mut bits = Bitmap::<{ World::STORAGE_SIZE }>::new();
                bits.invert();
                get_existing_indices::<T1>(&world, &mut bits);
                get_existing_indices::<T2>(&world, &mut bits);
                get_existing_indices::<T3>(&world, &mut bits);
                bits
            };

            for index in &world.entities {
                if indexed_entities.get(index.0) {
                    result.push((*index, (
                        read_from_storage::<T1>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T1>()).as_str()),
                        read_from_storage::<T2>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T2>()).as_str()),
                        read_from_storage::<T3>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T3>()).as_str()),
                    )));
                }
            }

            cache_query_result(&mut world, &result);
            result
        }
    }
}

impl<'a, T1, T2, T3, T4> QueryBundle for (T1, T2, T3, T4)
    where T1: Serialize + for<'de> Deserialize<'de>,
          T2: Serialize + for<'de> Deserialize<'de>,
          T3: Serialize + for<'de> Deserialize<'de>,
          T4: Serialize + for<'de> Deserialize<'de>,
{
    fn query() -> Vec<(Entity, Self)> {
        let mut world = WORLD.lock().unwrap();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res
        } else {
            let tid = generate_typeid::<(T1, T2, T3, T4)>();
            if !world.type_dependencies.contains(&tid) {
                world.type_dependencies.insert(generate_typeid::<T1>(), tid);
                world.type_dependencies.insert(generate_typeid::<T2>(), tid);
                world.type_dependencies.insert(generate_typeid::<T3>(), tid);
                world.type_dependencies.insert(generate_typeid::<T4>(), tid);
            }

            let mut result = Vec::new();
            let indexed_entities = {
                let mut bits = Bitmap::<{ World::STORAGE_SIZE }>::new();
                bits.invert();
                get_existing_indices::<T1>(&world, &mut bits);
                get_existing_indices::<T2>(&world, &mut bits);
                get_existing_indices::<T3>(&world, &mut bits);
                get_existing_indices::<T4>(&world, &mut bits);
                bits
            };

            for index in &world.entities {
                if indexed_entities.get(index.0) {
                    result.push((*index, (
                        read_from_storage::<T1>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T1>()).as_str()),
                        read_from_storage::<T2>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T2>()).as_str()),
                        read_from_storage::<T3>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T3>()).as_str()),
                        read_from_storage::<T4>(&world, *index)
                            .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T4>()).as_str()),
                    )));
                }
            }

            cache_query_result(&mut world, &result);
            result
        }
    }
}