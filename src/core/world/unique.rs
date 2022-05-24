use crate::world::*;
use serde::*;
use bitmaps::Bitmap;

pub trait GetUniqueBundle: Sized {
    fn get() -> Option<(Entity, Self)>;

    fn get_components() -> Option<Self> {
        Self::get().map(|(_, comps)| comps)
    }
}

impl Entity {
    pub fn get<T>(&self) -> Option<T>
        where for<'de> T: Serialize + Deserialize<'de> {

        let world = WORLD.lock().unwrap();
        read_from_storage::<T>(&world, *self)
    }
}

impl<T> GetUniqueBundle for (T,)
    where T: for<'de> Deserialize<'de> + std::clone::Clone
{
    fn get() -> Option<(Entity, Self)> {
        let world = WORLD.lock().unwrap();
        let tid = generate_typeid::<T>();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res.first().cloned()
        } else {
            for e in world.components.get(&tid).unwrap().first() {
                let index = Entity(e.key());
                return Some((index,
                             (read_from_storage::<T>(&world, index)
                                  .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T>()).as_str()), ),
                ));
            }

            None
        }
    }
}

impl<T1, T2> GetUniqueBundle for (T1, T2)
    where T1: for<'de> Deserialize<'de> + std::clone::Clone,
          T2: for<'de> Deserialize<'de> + std::clone::Clone,
{
    fn get() -> Option<(Entity, Self)> {
        let world = WORLD.lock().unwrap();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res.first().cloned()
        } else {
            let indexed_entities: Bitmap<{ World::STORAGE_SIZE }> = {
                let mut bits = Bitmap::<{ World::STORAGE_SIZE }>::new();
                bits.invert();
                get_existing_indices::<T1>(&world, &mut bits);
                get_existing_indices::<T2>(&world, &mut bits);
                bits
            };

            for index in indexed_entities.first_index() {
                let index = Entity(index);
                return Some((index, (
                    read_from_storage::<T1>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T1>()).as_str()),
                    read_from_storage::<T2>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T2>()).as_str()),
                )));
            }

            None
        }
    }
}

impl<T1, T2, T3> GetUniqueBundle for (T1, T2, T3)
    where T1: for<'de> Deserialize<'de> + std::clone::Clone,
          T2: for<'de> Deserialize<'de> + std::clone::Clone,
          T3: for<'de> Deserialize<'de> + std::clone::Clone,
{
    fn get() -> Option<(Entity, Self)> {
        let world = WORLD.lock().unwrap();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res.first().cloned()
        } else {
            let indexed_entities: Bitmap<{ World::STORAGE_SIZE }> = {
                let mut bits = Bitmap::<{ World::STORAGE_SIZE }>::new();
                bits.invert();
                get_existing_indices::<T1>(&world, &mut bits);
                get_existing_indices::<T2>(&world, &mut bits);
                get_existing_indices::<T3>(&world, &mut bits);
                bits
            };

            for index in indexed_entities.first_index() {
                let index = Entity(index);
                return Some((index, (
                    read_from_storage::<T1>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T1>()).as_str()),
                    read_from_storage::<T2>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T2>()).as_str()),
                    read_from_storage::<T3>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T3>()).as_str()),
                )));
            }

            None
        }
    }
}


impl<T1, T2, T3, T4> GetUniqueBundle for (T1, T2, T3, T4)
    where T1: for<'de> Deserialize<'de> + std::clone::Clone,
          T2: for<'de> Deserialize<'de> + std::clone::Clone,
          T3: for<'de> Deserialize<'de> + std::clone::Clone,
          T4: for<'de> Deserialize<'de> + std::clone::Clone,
{
    fn get() -> Option<(Entity, Self)> {
        let world = WORLD.lock().unwrap();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res.first().cloned()
        } else {
            let indexed_entities: Bitmap<{ World::STORAGE_SIZE }> = {
                let mut bits = Bitmap::<{ World::STORAGE_SIZE }>::new();
                bits.invert();
                get_existing_indices::<T1>(&world, &mut bits);
                get_existing_indices::<T2>(&world, &mut bits);
                get_existing_indices::<T3>(&world, &mut bits);
                get_existing_indices::<T4>(&world, &mut bits);
                bits
            };

            for index in indexed_entities.first_index() {
                let index = Entity(index);
                return Some((index, (
                    read_from_storage::<T1>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T1>()).as_str()),
                    read_from_storage::<T2>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T2>()).as_str()),
                    read_from_storage::<T3>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T3>()).as_str()),
                    read_from_storage::<T4>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T4>()).as_str()),
                )));
            }

            None
        }
    }
}

impl<T1, T2, T3, T4, T5> GetUniqueBundle for (T1, T2, T3, T4, T5)
    where T1: for<'de> Deserialize<'de> + std::clone::Clone,
          T2: for<'de> Deserialize<'de> + std::clone::Clone,
          T3: for<'de> Deserialize<'de> + std::clone::Clone,
          T4: for<'de> Deserialize<'de> + std::clone::Clone,
          T5: for<'de> Deserialize<'de> + std::clone::Clone,
{
    fn get() -> Option<(Entity, Self)> {
        let world = WORLD.lock().unwrap();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res.first().cloned()
        } else {
            let indexed_entities: Bitmap<{ World::STORAGE_SIZE }> = {
                let mut bits = Bitmap::<{ World::STORAGE_SIZE }>::new();
                bits.invert();
                get_existing_indices::<T1>(&world, &mut bits);
                get_existing_indices::<T2>(&world, &mut bits);
                get_existing_indices::<T3>(&world, &mut bits);
                get_existing_indices::<T4>(&world, &mut bits);
                get_existing_indices::<T5>(&world, &mut bits);
                bits
            };

            for index in indexed_entities.first_index() {
                let index = Entity(index);
                return Some((index, (
                    read_from_storage::<T1>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T1>()).as_str()),
                    read_from_storage::<T2>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T2>()).as_str()),
                    read_from_storage::<T3>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T3>()).as_str()),
                    read_from_storage::<T4>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T4>()).as_str()),
                    read_from_storage::<T5>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T5>()).as_str()),
                )));
            }

            None
        }
    }
}

impl<T1, T2, T3, T4, T5, T6> GetUniqueBundle for (T1, T2, T3, T4, T5, T6)
    where T1: for<'de> Deserialize<'de> + std::clone::Clone,
          T2: for<'de> Deserialize<'de> + std::clone::Clone,
          T3: for<'de> Deserialize<'de> + std::clone::Clone,
          T4: for<'de> Deserialize<'de> + std::clone::Clone,
          T5: for<'de> Deserialize<'de> + std::clone::Clone,
          T6: for<'de> Deserialize<'de> + std::clone::Clone,
{
    fn get() -> Option<(Entity, Self)> {
        let world = WORLD.lock().unwrap();

        if let Some(res) = get_cached_query_result::<Self>(&world) {
            res.first().cloned()
        } else {
            let indexed_entities: Bitmap<{ World::STORAGE_SIZE }> = {
                let mut bits = Bitmap::<{ World::STORAGE_SIZE }>::new();
                bits.invert();
                get_existing_indices::<T1>(&world, &mut bits);
                get_existing_indices::<T2>(&world, &mut bits);
                get_existing_indices::<T3>(&world, &mut bits);
                get_existing_indices::<T4>(&world, &mut bits);
                get_existing_indices::<T5>(&world, &mut bits);
                get_existing_indices::<T6>(&world, &mut bits);
                bits
            };

            for index in indexed_entities.first_index() {
                let index = Entity(index);
                return Some((index, (
                    read_from_storage::<T1>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T1>()).as_str()),
                    read_from_storage::<T2>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T2>()).as_str()),
                    read_from_storage::<T3>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T3>()).as_str()),
                    read_from_storage::<T4>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T4>()).as_str()),
                    read_from_storage::<T5>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T5>()).as_str()),
                    read_from_storage::<T6>(&world, index)
                        .expect(format!("Couldn't get component at index {} from {}", index.0, std::any::type_name::<T5>()).as_str()),
                )));
            }

            None
        }
    }
}

