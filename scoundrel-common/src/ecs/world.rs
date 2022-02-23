
use std::collections::{HashMap, HashSet};
use arena_rs::Arena;
use bitmaps::Bitmap;
use sparseset::SparseSet;
use crate::ecs::parser::{ComponentSignature, ComponentType, RascalValue};

type EntityId = usize;
type ComponentId = String;

const MAX_ENTRIES_PER_STORAGE: usize = 1024;
const MAX_COMPONENTS: usize = 1024;
const AVERAGE_COMPONENT_SIZE: usize = 50;
const JUST_TO_BE_SAFE_FACTOR: usize = 5;
const ARENA_SIZE: usize =
    MAX_ENTRIES_PER_STORAGE
        * MAX_COMPONENTS
        * AVERAGE_COMPONENT_SIZE
        * JUST_TO_BE_SAFE_FACTOR;

pub struct World {
    entity_count: usize,
    deleted_entities: Vec<EntityId>,
    component_types: HashMap<ComponentId, ComponentType>,
    registered_states: HashMap<ComponentId, ComponentSignature>,
    registered_events: HashMap<ComponentId, ComponentSignature>,
    registered_tags: HashSet<ComponentId>,
    pub storage_pointers: HashMap<ComponentId, Vec<u8>>,
    storage_bitmaps: HashMap<ComponentId, Bitmap<MAX_ENTRIES_PER_STORAGE>>,
}

impl Default for World {
    fn default() -> Self {
        World {
            entity_count: usize::default(),
            deleted_entities: Vec::default(),
            component_types: HashMap::default(),
            registered_states: HashMap::default(),
            registered_events: HashMap::default(),
            registered_tags: HashSet::default(),
            storage_pointers: HashMap::default(),
            storage_bitmaps: HashMap::default(),
        }
    }
}

impl World {
    pub fn create_entity(&mut self) -> EntityId {
        if self.deleted_entities.is_empty() {
            let entity = self.entity_count;
            self.entity_count += 1;
            entity
        } else {
            self.deleted_entities.pop().unwrap()
            // TODO: take care to delete previous allocations?
        }
    }

    pub fn add_component(&mut self, entity: EntityId, comp_type: ComponentId, comp_value: Vec<u8>) {
        assert!(entity < self.entity_count);
        assert!(self.component_types.contains_key(&comp_type));
        assert!(self.storage_bitmaps.contains_key(&comp_type));

        if self.component_types[&comp_type] != ComponentType::Tag {
            use ComponentType::*;
            assert!(self.storage_pointers.contains_key(&comp_type));

            let size = match self.component_types[&comp_type] {
                State => { self.registered_states[&comp_type].size as usize }
                Event => { self.registered_events[&comp_type].size as usize }
                Tag => unreachable!()
            };
            assert_eq!(size, comp_value.len());

            unsafe {
                let mut comp_storage_ptr = self.storage_pointers.get_mut(&comp_type).unwrap().as_mut_ptr();
                let comp_storage_at_entity_ptr = comp_storage_ptr.offset(entity as isize * size as isize);
                std::ptr::copy_nonoverlapping(comp_value.as_ptr(), comp_storage_at_entity_ptr, size);
            }
        }

        self.storage_bitmaps.get_mut(&comp_type).unwrap().set(entity, true);
    }

    pub fn register_component(&mut self, v: RascalValue) {
        fn create_storage(element_size: usize) -> Vec<u8> {
            let cap = MAX_ENTRIES_PER_STORAGE * MAX_COMPONENTS * element_size as usize;
            let mut storage = Vec::with_capacity(cap);
            for _ in 0..cap {
                storage.push(0u8);
            }
            storage
        }

        match v {
            RascalValue::State(comp) => {
                self.component_types.insert(comp.name.clone(), ComponentType::State);
                self.storage_pointers.insert(comp.name.clone(), create_storage(comp.size as usize));
                self.storage_bitmaps.insert(comp.name.clone(), Bitmap::new());
                self.registered_states.insert(comp.name.clone(), comp);
            }

            RascalValue::Event(comp) => {
                self.component_types.insert(comp.name.clone(), ComponentType::Event);
                self.storage_pointers.insert(comp.name.clone(), create_storage(comp.size as usize));
                self.storage_bitmaps.insert(comp.name.clone(), Bitmap::new());
                self.registered_events.insert(comp.name.clone(), comp);
            }

            RascalValue::Tag(tag) => {
                self.component_types.insert(tag.clone(), ComponentType::Tag);
                self.storage_bitmaps.insert(tag.clone(), Bitmap::new());
                // no storage pointer here, bitmap only!
                self.registered_tags.insert(tag);
            }
        };
    }
}