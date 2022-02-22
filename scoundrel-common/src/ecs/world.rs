
use std::collections::HashMap;
use sparseset::SparseSet;

type EntityId = usize;
type ComponentId = String;

#[derive(Default)]
pub struct World {
    entity_count: usize,
    contents: HashMap<ComponentId, SparseSet<ComponentId>>,
    storage: HashMap<ComponentId, Vec<u8>>,
}

impl World {
    pub fn create_entity(&mut self) -> EntityId {
        let current_entity = self.entity_count;
        self.entity_count += 1;

        current_entity
    }

    pub fn add_component(&mut self, entity: EntityId, comp: ComponentId) {
        assert!(entity < self.entity_count);
        if !self.contents.contains_key(&comp) {
            self.contents.insert(comp.clone(), SparseSet::with_capacity(128));
        }

        self.contents.get_mut(&comp).unwrap().insert(entity, comp);
    }

    pub fn add_components(&mut self, entity: EntityId, comps: Vec<ComponentId>) {
        for comp in comps {
            self.add_component(entity, comp);
        }
    }
}