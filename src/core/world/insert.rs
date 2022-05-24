use std::any::type_name;
use serde::Serialize;
use crate::core::world::contains::HasComponentBundle;
use crate::world::*;

pub trait InsertionBundle {
    fn insert(&self, world: &mut World, entity: Entity);
}

impl Entity {
    pub fn insert<BundledComponents: InsertionBundle + HasComponentBundle>(&self, cs: BundledComponents) {
        let mut world = WORLD.lock().unwrap();
        if BundledComponents::has_component(&world, *self) {
            println!("[SCOUNDREL ERROR] You should use `update` instead of `insert` if the entity already contains a component.");
            assert!(false);
        }
        cs.insert(&mut world, *self);
    }

    pub fn update<BundledComponents: InsertionBundle + HasComponentBundle>(&self, cs: BundledComponents) {
        let mut world = WORLD.lock().unwrap();
        if !BundledComponents::has_component(&world, *self) {
            println!("[SCOUNDREL ERROR] Cannot update component as it wasn't inserted into the entity:");
            println!("\n        {:?}\n", type_name::<BundledComponents>());
            println!("  Hint: This is a small error! Check that you're not updating a reference or a part of a struct!");
            println!("  For example...");
            println!("  >   let (level_entity, (MapDoors(mut doors), _)) = <(MapDoors, CurrentLevelTag)>::get().unwrap();");
            println!("  >                    // ^^^^^^^^^^^^^^^^^^^ -- we deconstructed MapDoors");
            println!("  >   doors.remove_at(&point);");
            println!("  >   level_entity.update(doors);");
            println!("  >             // ^^^^^^^^^^^^^ -- but updating the interior structure instead of MapDoors itself!\n");
            println!("  To fix this, we should wrap it:");
            println!("  >   level_entity.update(MapDoors(doors));");
            println!("  >                    // ^^^^^^^^^^^^^^^ -- okay now as the entity contains MapDoors\n");

            assert!(false);
        }
        cs.insert(&mut world, *self);
    }
}

impl World {
    pub fn spawn<BundledComponents: InsertionBundle>(cs: BundledComponents) -> Entity {
        let mut world = WORLD.lock().unwrap();
        let entity = Entity(world.last_entity);

        world.entities.insert(entity);
        cs.insert(&mut world, entity);
        println!("Spawning new entity {:?}", entity);
        world.last_entity += 1;

        entity
    }
}

impl<T> InsertionBundle for (T,)
    where T: Serialize {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1,) = self;

        create_storage::<T>(world);

        copy_into_storage::<T>(world, c1, entity);
        clear_query_result_cache::<T>(world);
    }
}

impl<T1, T2> InsertionBundle for (T1, T2)
    where T1: Serialize,
          T2: Serialize, {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1, c2) = self;

        create_storage::<T1>(world);
        create_storage::<T2>(world);

        copy_into_storage(world, c1, entity);
        copy_into_storage(world, c2, entity);

        clear_query_result_cache::<T1>(world);
        clear_query_result_cache::<T2>(world);
    }
}

impl<T1, T2, T3> InsertionBundle for (T1, T2, T3)
    where T1: Serialize,
          T2: Serialize,
          T3: Serialize, {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1, c2, c3) = self;

        create_storage::<T1>(world);
        create_storage::<T2>(world);
        create_storage::<T3>(world);

        copy_into_storage(world, c1, entity);
        copy_into_storage(world, c2, entity);
        copy_into_storage(world, c3, entity);

        clear_query_result_cache::<T1>(world);
        clear_query_result_cache::<T2>(world);
        clear_query_result_cache::<T3>(world);
    }
}

impl<T1, T2, T3, T4> InsertionBundle for (T1, T2, T3, T4)
    where T1: Serialize,
          T2: Serialize,
          T3: Serialize,
          T4: Serialize, {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1, c2, c3, c4) = self;

        create_storage::<T1>(world);
        create_storage::<T2>(world);
        create_storage::<T3>(world);
        create_storage::<T4>(world);

        copy_into_storage(world, c1, entity);
        copy_into_storage(world, c2, entity);
        copy_into_storage(world, c3, entity);
        copy_into_storage(world, c4, entity);

        clear_query_result_cache::<T1>(world);
        clear_query_result_cache::<T2>(world);
        clear_query_result_cache::<T3>(world);
        clear_query_result_cache::<T4>(world);
    }
}

impl<T1, T2, T3, T4, T5> InsertionBundle for (T1, T2, T3, T4, T5)
    where T1: Serialize,
          T2: Serialize,
          T3: Serialize,
          T4: Serialize,
          T5: Serialize, {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1, c2, c3, c4, c5) = self;

        create_storage::<T1>(world);
        create_storage::<T2>(world);
        create_storage::<T3>(world);
        create_storage::<T4>(world);
        create_storage::<T5>(world);

        copy_into_storage(world, c1, entity);
        copy_into_storage(world, c2, entity);
        copy_into_storage(world, c3, entity);
        copy_into_storage(world, c4, entity);
        copy_into_storage(world, c5, entity);

        clear_query_result_cache::<T1>(world);
        clear_query_result_cache::<T2>(world);
        clear_query_result_cache::<T3>(world);
        clear_query_result_cache::<T4>(world);
        clear_query_result_cache::<T5>(world);
    }
}

impl<T1, T2, T3, T4, T5, T6> InsertionBundle for (T1, T2, T3, T4, T5, T6)
    where T1: Serialize,
          T2: Serialize,
          T3: Serialize,
          T4: Serialize,
          T5: Serialize,
          T6: Serialize, {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1, c2, c3, c4, c5, c6) = self;

        create_storage::<T1>(world);
        create_storage::<T2>(world);
        create_storage::<T3>(world);
        create_storage::<T4>(world);
        create_storage::<T5>(world);
        create_storage::<T6>(world);

        copy_into_storage(world, c1, entity);
        copy_into_storage(world, c2, entity);
        copy_into_storage(world, c3, entity);
        copy_into_storage(world, c4, entity);
        copy_into_storage(world, c5, entity);
        copy_into_storage(world, c6, entity);

        clear_query_result_cache::<T1>(world);
        clear_query_result_cache::<T2>(world);
        clear_query_result_cache::<T3>(world);
        clear_query_result_cache::<T4>(world);
        clear_query_result_cache::<T5>(world);
        clear_query_result_cache::<T6>(world);
    }
}

impl<T1, T2, T3, T4, T5, T6, T7> InsertionBundle for (T1, T2, T3, T4, T5, T6, T7)
    where T1: Serialize,
          T2: Serialize,
          T3: Serialize,
          T4: Serialize,
          T5: Serialize,
          T6: Serialize,
          T7: Serialize, {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1, c2, c3, c4, c5, c6, c7) = self;

        create_storage::<T1>(world);
        create_storage::<T2>(world);
        create_storage::<T3>(world);
        create_storage::<T4>(world);
        create_storage::<T5>(world);
        create_storage::<T6>(world);
        create_storage::<T7>(world);

        copy_into_storage(world, c1, entity);
        copy_into_storage(world, c2, entity);
        copy_into_storage(world, c3, entity);
        copy_into_storage(world, c4, entity);
        copy_into_storage(world, c5, entity);
        copy_into_storage(world, c6, entity);
        copy_into_storage(world, c7, entity);

        clear_query_result_cache::<T1>(world);
        clear_query_result_cache::<T2>(world);
        clear_query_result_cache::<T3>(world);
        clear_query_result_cache::<T4>(world);
        clear_query_result_cache::<T5>(world);
        clear_query_result_cache::<T6>(world);
        clear_query_result_cache::<T7>(world);
    }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8> InsertionBundle for (T1, T2, T3, T4, T5, T6, T7, T8)
    where T1: Serialize,
          T2: Serialize,
          T3: Serialize,
          T4: Serialize,
          T5: Serialize,
          T6: Serialize,
          T7: Serialize,
          T8: Serialize, {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1, c2, c3, c4, c5, c6, c7, c8) = self;

        create_storage::<T1>(world);
        create_storage::<T2>(world);
        create_storage::<T3>(world);
        create_storage::<T4>(world);
        create_storage::<T5>(world);
        create_storage::<T6>(world);
        create_storage::<T7>(world);
        create_storage::<T8>(world);

        copy_into_storage(world, c1, entity);
        copy_into_storage(world, c2, entity);
        copy_into_storage(world, c3, entity);
        copy_into_storage(world, c4, entity);
        copy_into_storage(world, c5, entity);
        copy_into_storage(world, c6, entity);
        copy_into_storage(world, c7, entity);
        copy_into_storage(world, c8, entity);

        clear_query_result_cache::<T1>(world);
        clear_query_result_cache::<T2>(world);
        clear_query_result_cache::<T3>(world);
        clear_query_result_cache::<T4>(world);
        clear_query_result_cache::<T5>(world);
        clear_query_result_cache::<T6>(world);
        clear_query_result_cache::<T7>(world);
        clear_query_result_cache::<T8>(world);
    }
}

impl<T1, T2, T3, T4, T5, T6, T7, T8, T9> InsertionBundle for (T1, T2, T3, T4, T5, T6, T7, T8, T9)
    where T1: Serialize,
          T2: Serialize,
          T3: Serialize,
          T4: Serialize,
          T5: Serialize,
          T6: Serialize,
          T7: Serialize,
          T8: Serialize,
          T9: Serialize, {

    fn insert(&self, world: &mut World, entity: Entity) {
        let (c1, c2, c3, c4, c5, c6, c7, c8, c9) = self;

        create_storage::<T1>(world);
        create_storage::<T2>(world);
        create_storage::<T3>(world);
        create_storage::<T4>(world);
        create_storage::<T5>(world);
        create_storage::<T6>(world);
        create_storage::<T7>(world);
        create_storage::<T8>(world);
        create_storage::<T9>(world);

        copy_into_storage(world, c1, entity);
        copy_into_storage(world, c2, entity);
        copy_into_storage(world, c3, entity);
        copy_into_storage(world, c4, entity);
        copy_into_storage(world, c5, entity);
        copy_into_storage(world, c6, entity);
        copy_into_storage(world, c7, entity);
        copy_into_storage(world, c8, entity);
        copy_into_storage(world, c9, entity);

        clear_query_result_cache::<T1>(world);
        clear_query_result_cache::<T2>(world);
        clear_query_result_cache::<T3>(world);
        clear_query_result_cache::<T4>(world);
        clear_query_result_cache::<T5>(world);
        clear_query_result_cache::<T6>(world);
        clear_query_result_cache::<T7>(world);
        clear_query_result_cache::<T8>(world);
        clear_query_result_cache::<T9>(world);
    }
}