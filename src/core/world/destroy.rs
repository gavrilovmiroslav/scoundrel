use crate::world::*;

impl Entity {
    pub fn despawn(&self) {
        let mut world = WORLD.lock().unwrap();
        world.entities.remove(self);

        let comps = world.component_index.clone();
        for (typeid, _) in comps {
            clear_query_result_cache_for_typeid(&mut world, typeid);
        }

        for (_, comps) in &mut world.component_index {
            comps.remove(self);
        }
    }
}
