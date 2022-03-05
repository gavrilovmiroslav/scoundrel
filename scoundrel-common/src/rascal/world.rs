use std::collections::{HashMap, HashSet};
use std::ptr::copy_nonoverlapping;
use std::sync::Mutex;

use bitmaps::Bitmap;
use lazy_static::lazy_static;
use priority_queue::PriorityQueue;
use show_my_errors::{AnnotationList, Stylesheet};

use crate::engine::WORLD;
use crate::rascal::parser::{ComponentSignature, ComponentType, RascalStruct, SystemSignature};
use crate::rascal::vm::{RascalValue, RascalVM};

pub(crate) type EntityId = usize;
pub(crate) type ComponentId = String;
type SystemId = String;

pub const MAX_ENTRIES_PER_STORAGE: usize = 1024;
const MAX_COMPONENTS: usize = 1024;
const AVERAGE_COMPONENT_SIZE: usize = 50;
const JUST_TO_BE_SAFE_FACTOR: usize = 5;
const ARENA_SIZE: usize =
    MAX_ENTRIES_PER_STORAGE
        * MAX_COMPONENTS
        * AVERAGE_COMPONENT_SIZE
        * JUST_TO_BE_SAFE_FACTOR;

pub struct World {
    pub component_names: Vec<ComponentId>,
    pub component_types: HashMap<ComponentId, ComponentType>,
    pub registered_states: HashMap<ComponentId, ComponentSignature>,
    pub registered_events: HashMap<ComponentId, ComponentSignature>,
    pub registered_tags: HashSet<ComponentId>,
    pub storage_pointers: HashMap<ComponentId, Vec<u8>>,
    pub entity_bitmaps: Bitmap<MAX_ENTRIES_PER_STORAGE>,
    pub storage_bitmaps: HashMap<ComponentId, Bitmap<MAX_ENTRIES_PER_STORAGE>>,
}

impl Default for World {
    fn default() -> Self {
        World {
            component_names: Vec::default(),
            component_types: HashMap::default(),
            registered_states: HashMap::default(),
            registered_events: HashMap::default(),
            registered_tags: HashSet::default(),
            storage_pointers: HashMap::default(),
            entity_bitmaps: Bitmap::new(),
            storage_bitmaps: HashMap::default(),
        }
    }
}

lazy_static! {
    pub static ref REGISTERED_SYSTEMS: Mutex<HashMap<SystemId, SystemSignature>> = Mutex::new(HashMap::default());
    pub static ref SYSTEM_PRIORITIES: Mutex<PriorityQueue<SystemId, u32>> = Mutex::new(PriorityQueue::new());
    pub static ref SYSTEM_QUERY_CACHE: Mutex<HashMap<u64, Bitmap<MAX_ENTRIES_PER_STORAGE>>> = Mutex::new(HashMap::new());
}

pub trait AddComponent<T> {
    fn add_component(&mut self, entity: EntityId, comp_type: &str, comp_value: Vec<T>);
}

pub fn world_contains_event(event_name: &str) -> bool {
    WORLD.lock().unwrap().contains_event(event_name)
}

impl World {
    pub fn contains_event(&self, event_name: &str) -> bool {
        !self.storage_bitmaps.get(&event_name.to_string()).unwrap().is_empty()
    }

    pub fn add_tag(&mut self, entity: EntityId, comp_type: &str) {
        let value: Vec<u8> = vec![];
        self.add_component(entity, comp_type, value);
    }

    pub fn run_all_systems(&mut self) {
        let mut vm = RascalVM::new();

        for sys in SYSTEM_PRIORITIES.lock().unwrap().clone().into_sorted_vec().iter().rev() {
            vm.interpret(self, REGISTERED_SYSTEMS.lock().unwrap().get(sys).unwrap());
        }
    }

    pub fn create_entity(&mut self) -> EntityId {
        let index = self.entity_bitmaps.first_false_index().unwrap_or(0) as EntityId;
        self.entity_bitmaps.set(index, true);
        index
    }

    pub fn register_system(&mut self, s: RascalStruct) {
        if let RascalStruct::System(sys) = s {
            let mut registered_systems = REGISTERED_SYSTEMS.lock().unwrap();

            let mut prio = SYSTEM_PRIORITIES.lock().unwrap();
            prio.push(sys.name.clone(), registered_systems.len() as u32);

            let sys_with_id = sys.with_id(registered_systems.len() as u64 + 1);
            registered_systems.insert(sys_with_id.name.clone(), sys_with_id);
            // TODO: save the events that trigger systems here, so that we don't have to run them all the time
        }
    }

    pub fn register_component(&mut self, v: RascalStruct) {
        fn create_storage(element_size: usize) -> Vec<u8> {
            let cap = MAX_ENTRIES_PER_STORAGE * MAX_COMPONENTS * element_size as usize;
            let mut storage = Vec::with_capacity(cap);
            storage.resize(cap, 0);
            storage
        }

        match v {
            RascalStruct::State(comp) => {
                self.component_types.insert(comp.name.clone(), ComponentType::State);
                self.storage_pointers.insert(comp.name.clone(), create_storage(comp.size as usize));
                self.storage_bitmaps.insert(comp.name.clone(), Bitmap::new());
                self.component_names.push(comp.name.clone());
                self.registered_states.insert(comp.name.clone(), comp);
            }

            RascalStruct::Event(comp) => {
                self.component_types.insert(comp.name.clone(), ComponentType::Event);
                self.storage_pointers.insert(comp.name.clone(), create_storage(comp.size as usize));
                self.storage_bitmaps.insert(comp.name.clone(), Bitmap::new());
                self.registered_events.insert(comp.name.clone(), comp);
            }

            RascalStruct::Tag(tag) => {
                self.component_types.insert(tag.clone(), ComponentType::Tag);
                self.storage_bitmaps.insert(tag.clone(), Bitmap::new());
                // no storage pointer here, bitmap only!
                self.registered_tags.insert(tag.clone());
                self.component_names.push(tag.clone());
            }

            _ => unreachable!()
        };
    }
}

impl AddComponent<u8> for World {
    fn add_component(&mut self, entity: EntityId, comp_type: &str, comp_value: Vec<u8>) {
        let comp_type = comp_type.to_string();

        if !self.component_types.contains_key(&comp_type) {
            let mut list = AnnotationList::new("world.rs", "!self.component_types.contains_key(&comp_type)");
            list.error(36..39, format!("Component type storage for {} not registered", comp_type), "this really should be a warning later on"); // TODO!
            list.show_stdout(&Stylesheet::colored());
            return;
        }

        if !self.storage_bitmaps.contains_key(&comp_type) {
            let mut list = AnnotationList::new("world.rs", "!self.storage_bitmaps.contains_key(&comp_type)");
            list.error(35..38, format!("Component type bitmap for {} not registered", comp_type), "this really should be a warning later on"); // TODO!
            list.show_stdout(&Stylesheet::colored());
            return;
        }

        if self.component_types[&comp_type] != ComponentType::Tag {
            use ComponentType::*;

            let size = match self.component_types[&comp_type] {
                State => { self.registered_states[&comp_type].size as usize }
                Event => { self.registered_events[&comp_type].size as usize }
                _ => 0
            };

            unsafe {
                let mut comp_storage_ptr = self.storage_pointers.get_mut(&comp_type).unwrap().as_mut_ptr();
                let comp_storage_at_entity_ptr = comp_storage_ptr.offset(entity as isize * size as isize);
                copy_nonoverlapping(comp_value.as_ptr(), comp_storage_at_entity_ptr, size);
            }
        }

        self.storage_bitmaps.get_mut(&comp_type).unwrap().set(entity, true);
    }
}

impl AddComponent<RascalValue> for World {
    fn add_component(&mut self, entity: EntityId, comp_type: &str, comp_value: Vec<RascalValue>) {
        let mut result = Vec::new();
        for comp in comp_value {
            result.extend_from_slice(comp.emit().as_slice());
        }

        self.add_component(entity, comp_type, result);
    }
}
