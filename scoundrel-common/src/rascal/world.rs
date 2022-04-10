use std::collections::{HashMap, HashSet, VecDeque};
use std::path::Path;
use std::ptr::copy_nonoverlapping;
use std::sync::Mutex;

use bitmaps::Bitmap;
use caves::{Cave, FileCave};
use lazy_static::lazy_static;
use priority_queue::PriorityQueue;
use rand_chacha::ChaCha20Rng;
use rand_chacha::rand_core::SeedableRng;
use show_my_errors::{AnnotationList, Stylesheet};

use crate::engine::{set_should_redraw, WORLD};
use crate::rascal::interpreter::{Geom, get_or_insert_into_string_pool, num, RascalEventfulResult, RascalValue, RascalVM};
use crate::rascal::parser::{ComponentSignature, ComponentType, DataType, RascalStruct, SystemPriority, SystemPrioritySize, SystemSignature};
use crate::readonly_archive_cave::ReadonlyArchiveCave;

use crate::rascal::parser::ProcSignature;

pub(crate) type EntityId = usize;
pub(crate) type ComponentId = String;
type SystemId = String;
type ProcId = String;

pub const MAX_ENTRIES_PER_STORAGE: usize = 1024;
const MAX_COMPONENTS: usize = 1024;
const AVERAGE_COMPONENT_SIZE: usize = 50;
const JUST_TO_BE_SAFE_FACTOR: usize = 5;
const ARENA_SIZE: usize =
    MAX_ENTRIES_PER_STORAGE
        * MAX_COMPONENTS
        * AVERAGE_COMPONENT_SIZE
        * JUST_TO_BE_SAFE_FACTOR;

pub type BinaryComponent = Vec<u8>;

pub type FieldId = u32;
pub type SetId = u32;

pub struct Field {
    pub datatype: DataType,
    pub field_map: HashMap<usize, RascalValue>,
}

pub struct World {
    pub random: ChaCha20Rng,
    pub data: Box<dyn Cave>,
    pub definitions_in_source: HashMap<String, Vec<(ComponentId, ComponentType)>>,
    pub component_names: Vec<ComponentId>,
    pub component_types: HashMap<ComponentId, ComponentType>,
    pub registered_states: HashMap<ComponentId, ComponentSignature>,
    pub registered_events: HashMap<ComponentId, ComponentSignature>,
    pub registered_tags: HashSet<ComponentId>,
    pub storage_pointers: HashMap<ComponentId, BinaryComponent>,
    pub entity_bitmap: Bitmap<MAX_ENTRIES_PER_STORAGE>,
    pub event_queues: HashMap<ComponentId, VecDeque<BinaryComponent>>,
    pub next_event_queues: HashMap<ComponentId, VecDeque<BinaryComponent>>,
    pub storage_bitmaps: HashMap<ComponentId, Bitmap<MAX_ENTRIES_PER_STORAGE>>,
    pub system_priorities: HashMap<SystemPriority, u16>,
    pub unique_storage: HashMap<u32, RascalValue>,
    pub field_storage: HashMap<FieldId, Field>,
    pub set_storage: HashMap<SetId, HashSet<(i32, i32)>>,
    pub set_cache: HashMap<Geom, HashSet<(i32, i32)>>,
    pub size: (u32, u32),
}

impl Default for World {
    fn default() -> Self {
        World {
            random: ChaCha20Rng::seed_from_u64(0u64),
            data: if cfg!(debug_assertions) {
                ReadonlyArchiveCave::make_from("resources/data", "resources/data.bin");
                Box::new(FileCave::new(Path::new("resources/data")).unwrap())
            } else {
                Box::new(ReadonlyArchiveCave::open("resources/data.bin"))
            },
            definitions_in_source: HashMap::default(),
            component_names: Vec::default(),
            component_types: HashMap::default(),
            registered_states: HashMap::default(),
            registered_events: HashMap::default(),
            registered_tags: HashSet::default(),
            storage_pointers: HashMap::default(),
            entity_bitmap: Bitmap::new(),
            event_queues: HashMap::default(),
            next_event_queues: HashMap::default(),
            storage_bitmaps: HashMap::default(),
            system_priorities: HashMap::default(),
            unique_storage: HashMap::default(),
            field_storage: HashMap::default(),
            set_storage: HashMap::default(),
            set_cache: HashMap::default(),
            size: (0, 0),
        }
    }
}

const RESERVED_FIRST_PRIORITY: SystemPrioritySize = 1000;

lazy_static! {
    pub static ref REGISTERED_SYSTEMS: Mutex<HashMap<SystemId, SystemSignature>> = Mutex::new(HashMap::default());
    pub static ref REGISTERED_PROCS: Mutex<HashMap<ProcId, ProcSignature>> = Mutex::new(HashMap::default());
    pub static ref SYSTEM_DEPENDENCIES: Mutex<HashMap<ComponentId, PriorityQueue<SystemId, SystemPrioritySize>>> = Mutex::new(HashMap::default());
    pub static ref CACHED_SYSTEMS_BY_PRIORITIES: Mutex<HashMap<ComponentId, Vec<SystemId>>> = Mutex::new(HashMap::default());
    pub static ref SYSTEM_QUERY_CACHE: Mutex<HashMap<u64, Bitmap<MAX_ENTRIES_PER_STORAGE>>> = Mutex::new(HashMap::new());
}

pub trait AddComponent<T> {
    fn add_component(&mut self, entity: EntityId, comp_type: &str, comp_value: Vec<T>);
}

pub trait TriggerEvent<T> {
    fn trigger_event(&mut self, comp_type: &str, comp_value: Vec<T>);
}

pub fn send_start_event() {
    println!("START");
    let mut world = WORLD.lock().unwrap();

    let main_entity = world.create_entity();
    world.add_tag(main_entity, "Main");

    world.trigger_event("Start", vec![ num(0) ]);
}

pub fn world_contains_event(event_name: &str) -> bool {
    WORLD.lock().unwrap().contains_event(event_name)
}

impl World {
    pub fn clear_entities(&mut self) {
        for (_, b) in &mut self.storage_bitmaps {
            *b = Bitmap::new();
        }

        self.entity_bitmap = Bitmap::new();
    }

    pub fn contains_event(&self, event_name: &str) -> bool {
        !self.storage_bitmaps.get(&event_name.to_string()).unwrap().is_empty()
    }

    pub fn add_tag(&mut self, entity: EntityId, comp_type: &str) {
        let value: Vec<u8> = vec![];
        self.add_component(entity, comp_type, value);
    }

    pub fn run_all_systems(&mut self) {
        let mut vm = RascalVM::new();

        // dbg!("Start of frame ----------------------------------------");
        let events = SYSTEM_DEPENDENCIES.lock().unwrap();
        let systems = REGISTERED_SYSTEMS.lock().unwrap();
        let cached_dependencies = CACHED_SYSTEMS_BY_PRIORITIES.lock().unwrap();
        for (event_name, dependent_systems) in &*events {
            if *event_name == "Tick" { continue; }
            if let Some(original_event_queue) = self.event_queues.get(event_name) {
                let mut event_queue = original_event_queue.clone();
                let event_signature = self.registered_events.get(event_name).unwrap().clone();
                while let Some(event) = event_queue.pop_front() {
                    for sys in cached_dependencies.get(event_name).unwrap() {
                        let consume_result = vm.interpret_eventful(self,
                                              systems.get(sys).unwrap(),
                                              &event_signature, event.clone());

                        if let Some(RascalEventfulResult::Consumed) = consume_result {
                            break;
                        }
                    }
                }
            } else {
                println!("No queue for {}.", event_name);
                unreachable!();
            }

            self.event_queues.get_mut(event_name).unwrap().clear();
            if !self.next_event_queues.get(event_name).unwrap().is_empty() {
                self.event_queues.get_mut(event_name).unwrap().append(self.next_event_queues.get_mut(event_name).unwrap());
            }
        }

        // dbg!("on tick:");
        if let Some(tick_systems) = events.get(&"Tick".to_string()) {
            for (system, _prio) in tick_systems.clone().into_sorted_iter() {
                // dbg!("* {}: {}", prio, system);

                vm.interpret_stateful(self, systems.get(&system).unwrap());
            }
        }

        if vm.is_something_printed() {
            set_should_redraw();
        }
    }

    pub fn create_entity(&mut self) -> EntityId {
        let index = self.entity_bitmap.first_false_index().unwrap_or(0) as EntityId;
        self.entity_bitmap.set(index, true);
        index
    }

    pub fn register_system(&mut self, s: RascalStruct) {
        if let RascalStruct::System(sys) = s {
            if !self.system_priorities.contains_key(&sys.priority) {
                self.system_priorities.insert(sys.priority.clone(), 0);
            }

            let p = self.system_priorities.get_mut(&sys.priority).unwrap();
            *p += 1;

            let priority = match sys.priority {
                SystemPriority::Default => RESERVED_FIRST_PRIORITY + *p as SystemPrioritySize,
                SystemPriority::Last => (SystemPrioritySize::MAX - RESERVED_FIRST_PRIORITY) + *p,
                SystemPriority::Game => (SystemPrioritySize::MAX - 3 * RESERVED_FIRST_PRIORITY) + *p,
                SystemPriority::UI => (SystemPrioritySize::MAX - 2 * RESERVED_FIRST_PRIORITY) + *p,
                SystemPriority::First => *p,
                SystemPriority::At(level) => RESERVED_FIRST_PRIORITY + level,
            };

            println!("System {} registered at priority level {} ({:?})", sys.name, priority, sys.priority);

            let mut registered_systems = REGISTERED_SYSTEMS.lock().unwrap();
            let mut sys_with_id = sys.with_id(registered_systems.len() as u64 + 1);
            sys_with_id.real_priority = priority;
            let depends_on_event_queue = sys_with_id.clone().activation_event.map(|e| e.name).unwrap_or("Tick".to_string());

            if !SYSTEM_DEPENDENCIES.lock().unwrap().contains_key(&depends_on_event_queue) {
                SYSTEM_DEPENDENCIES.lock().unwrap().insert(depends_on_event_queue.clone(), PriorityQueue::new());
            }

            let mut system_dependencies = SYSTEM_DEPENDENCIES.lock().unwrap();
            let mut dependencies = system_dependencies.get_mut(&depends_on_event_queue).unwrap();
            dependencies.push(sys_with_id.name.clone(), SystemPrioritySize::MAX - priority);
            registered_systems.insert(sys_with_id.name.clone(), sys_with_id);
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
            RascalStruct::State(state) => {
                self.component_types.insert(state.name.clone(), ComponentType::State);
                self.storage_pointers.insert(state.name.clone(), create_storage(state.size as usize));
                self.storage_bitmaps.insert(state.name.clone(), Bitmap::new());
                self.component_names.push(state.name.clone());
                self.registered_states.insert(state.name.clone(), state);
            }

            RascalStruct::Event(event) => {
                self.component_types.insert(event.name.clone(), ComponentType::Event);
                self.event_queues.insert(event.name.clone(), VecDeque::default());
                self.next_event_queues.insert(event.name.clone(), VecDeque::default());
                println!("Registered event queue for {}", event.name);
                self.registered_events.insert(event.name.clone(), event);
            }

            RascalStruct::Tag(tag) => {
                self.component_types.insert(tag.clone(), ComponentType::Tag);
                self.storage_bitmaps.insert(tag.clone(), Bitmap::new());
                // no storage pointer here, bitmap only!
                self.registered_tags.insert(tag.clone());
                self.component_names.push(tag.clone());
            }

            RascalStruct::Unique(name, datatype, subtype, value) => {
                let index = get_or_insert_into_string_pool(&name);
                if let DataType::Field = datatype {
                    self.field_storage.insert(index, Field { datatype: subtype.unwrap(), field_map: Default::default() });
                }
                self.unique_storage.insert(index, value);
            }

            RascalStruct::Proc(name, params, block) => {
                if !REGISTERED_PROCS.lock().unwrap().contains_key(&name) {
                    REGISTERED_PROCS.lock().unwrap().insert(name.clone(), ProcSignature {
                        name, params: HashMap::from_iter(params.into_iter()), block
                    });
                }
            }

            _ => unreachable!()
        };
    }
}

impl AddComponent<u8> for World {
    fn add_component(&mut self, entity: EntityId, comp_type: &str, comp_value: Vec<u8>) {
        assert_ne!(self.component_types[comp_type], ComponentType::Event);
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

impl TriggerEvent<u8> for World {
    fn trigger_event(&mut self, comp_type: &str, comp_value: Vec<u8>) {
        if let Some(mut queue) = self.next_event_queues.get_mut(comp_type) {
            queue.push_back(comp_value);
        }
    }
}

impl TriggerEvent<RascalValue> for World {
    fn trigger_event(&mut self, comp_type: &str, comp_value: Vec<RascalValue>) {
        let mut result = Vec::new();
        for comp in comp_value {
            result.extend_from_slice(comp.emit().as_slice());
        }

        self.trigger_event(comp_type, result);
    }
}