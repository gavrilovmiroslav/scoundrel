use crate::world::*;

pub trait HasComponentBundle: Sized {
    fn has_component(world: &World, entity: Entity) -> bool;
}

impl World {
    pub fn has_component(&self, tid: TypeId, entity: Entity) -> bool {
        if let Some(comps) = self.component_index.get(&tid) {
            comps.contains(&entity)
        } else {
            false
        }
    }
}

impl Entity {
    pub fn has_component<BundledComponents: HasComponentBundle>(&self) -> bool {
        let mut world = WORLD.lock().unwrap();
        BundledComponents::has_component(&mut world, *self)
    }
}

impl<T> HasComponentBundle for (T,) {
    fn has_component(world: &World, entity: Entity) -> bool {
        let t1 = generate_typeid::<T>();
        world.has_component(t1, entity)
    }
}

impl<T1, T2> HasComponentBundle for (T1, T2) {
    fn has_component(world: &World, entity: Entity) -> bool {
        let t1 = generate_typeid::<T1>();
        let t2 = generate_typeid::<T2>();

        let b1 = world.has_component(t1, entity);
        let b2 = world.has_component(t2, entity);

        b1 && b2
    }
}

impl<T1, T2, T3> HasComponentBundle for (T1, T2, T3) {
    fn has_component(world: &World, entity: Entity) -> bool {
        let t1 = generate_typeid::<T1>();
        let t2 = generate_typeid::<T2>();
        let t3 = generate_typeid::<T3>();

        let b1 = world.has_component(t1, entity);
        let b2 = world.has_component(t2, entity);
        let b3 = world.has_component(t3, entity);

        b1 && b2 && b3
    }
}

impl<T1, T2, T3, T4> HasComponentBundle for (T1, T2, T3, T4) {
    fn has_component(world: &World, entity: Entity) -> bool {
        let t1 = generate_typeid::<T1>();
        let t2 = generate_typeid::<T2>();
        let t3 = generate_typeid::<T3>();
        let t4 = generate_typeid::<T4>();

        let b1 = world.has_component(t1, entity);
        let b2 = world.has_component(t2, entity);
        let b3 = world.has_component(t3, entity);
        let b4 = world.has_component(t4, entity);

        b1 && b2 && b3 && b4
    }
}

