use std::collections::HashMap;
use std::ops::{BitAnd, Not};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use bitmaps::*;
use lazy_static::lazy_static;

use crate::rascal::parser::{ComponentCallSite, ComponentType, RascalBlock, RascalExpression, RascalStatement, SystemSignature};
use crate::rascal::parser::MemberId;
use crate::rascal::world::ComponentId;
use crate::rascal::world::MAX_ENTRIES_PER_STORAGE;
use crate::rascal::world::World;

lazy_static! {
    static ref SYSTEM_QUERY_CACHE: Mutex<HashMap<u64, Bitmap<MAX_ENTRIES_PER_STORAGE>>> = Mutex::new(HashMap::new());
}

#[derive(Debug)]
pub enum EqualityWith {
    Name(String),
    Value(RascalExpression),
}

#[derive(Debug)]
pub struct RascalVM {
    pub bindings: HashMap<String, (ComponentId, MemberId)>,
    pub equalities: Vec<(String, EqualityWith)>,
}

impl RascalVM {
    pub fn new() -> Self {
        RascalVM {
            bindings: HashMap::default(),
            equalities: Vec::default(),
        }
    }

    pub fn query_storages(&mut self, world: &World, comps: &Vec<ComponentCallSite>) -> Bitmap<MAX_ENTRIES_PER_STORAGE> {
        let empty_map = Bitmap::<MAX_ENTRIES_PER_STORAGE>::new().not();
        comps.iter().fold(empty_map, |b, c| {
            assert!(c.is_owned);
            let name = c.name.clone();

            let storage = world.storage_bitmaps.lock().unwrap();
            let bitmap = storage.get(&name).unwrap();
            if c.not {
                bitmap.not().bitand(b)
            } else {
                bitmap.bitand(b)
            }
        })
    }

    pub fn check_latest_equality(&self, system_name: &String, world: &World) -> Result<(), (u8, u8)> {
        fn get_size(world: &World, comp: &ComponentId, member: &MemberId) -> u8 {
            match world.component_types.get(comp).unwrap() {
                ComponentType::State =>
                    world.registered_states.get(comp).unwrap().members[member].typ.size_in_bytes(),
                ComponentType::Tag => 0u8,
                _ => unreachable!()
            }
        }

        if let Some((lhs, EqualityWith::Name(rhs))) = self.equalities.last() {
            let (left_comp, left_member) = self.bindings.get(lhs).unwrap();
            let (right_comp, right_member) = self.bindings.get(rhs).unwrap();

            let left_size = get_size(world, left_comp, left_member);
            let right_size = get_size(world, right_comp, right_member);
            if left_size != right_size {
                println!("ERROR [{}]: Two variables that are matched have different types ({} aka {}.{}, and {} aka {}.{})",
                         system_name, lhs, left_comp, left_member, rhs, right_comp, right_member);
                return Result::Err((left_size, right_size));
            }
        }

        Result::Ok(())
    }

    pub fn interpret_with(&mut self, world: &World, system: &SystemSignature) {
        println!("========== SYSTEM [{}] ===========", system.name);

        let mut binding_id = AtomicU64::new(1);

        /* CHECK FOR MULTIPLE OWNERS */{
            let with = system.with.clone();
            let mut owners: Vec<_> = with.iter().map(|o| o.owner.clone().unwrap_or(String::default())).collect();
            owners.sort();
            owners.dedup();
            if owners.len() > 1 {
                println!("ERROR [{}]: more than one owning entity per system is not supported yet. Qutting.", system.name);
                println!("DEBUG: owners = {:?}", owners);
                return;
            }
        }

        let query_bitmap = self.query_storages(world, &system.with);

        for call_site in system.with.clone() {
            let comp_type = world.component_types.get(&call_site.name).unwrap();
            match comp_type {
                ComponentType::State => {
                    let comp_signature = world.registered_states.get(&call_site.name).unwrap();
                    if call_site.args.len() != comp_signature.members.len() {
                        println!("[System {}] Component {} has {} parameters, but only {} arguments found",
                                 system.name, call_site.name, comp_signature.members.len(), call_site.args.len());
                        return;
                    }

                    for (arg, (_, id)) in call_site.args.iter().zip(comp_signature.ptrs.iter().collect::<Vec<_>>()) {
                        if arg.name == "_" && arg.value.is_some() {
                            let new_id = format!("${}_{}", arg.name, binding_id.fetch_add(1, Ordering::SeqCst));
                            self.equalities.push((new_id, EqualityWith::Value(arg.value.clone().unwrap())));
                        } else if arg.name != "_" && self.bindings.contains_key(&arg.name) {
                            let new_id = format!("${}_{}", arg.name, binding_id.fetch_add(1, Ordering::SeqCst));
                            self.bindings.insert(new_id.clone(), (comp_signature.name.clone(), id.clone()));
                            self.equalities.push((new_id, EqualityWith::Name(arg.name.clone())));

                            if self.check_latest_equality(&system.name, world).is_err() {
                                println!("[System {}] Quitting due to equality errors.", system.name);
                                return;
                            }
                        } else if arg.name != "_" {
                            self.bindings.insert(arg.name.clone(), (comp_signature.name.clone(), id.clone()));
                            if arg.value.is_some() {
                                self.equalities.push((arg.name.clone(), EqualityWith::Value(arg.value.clone().unwrap())));
                            }
                        }
                    }
                },

                ComponentType::Tag => {
                    if call_site.args.len() > 0 {
                        println!("[System {}] Tag {} has no parameters but {} supplied.",
                                 system.name, call_site.name, call_site.args.len());
                        return;
                    }
                },
                _ => {}
            }
        }

        for index in 0..MAX_ENTRIES_PER_STORAGE {
            if query_bitmap.get(index as usize) {
                println!("SYSTEM [{}]: working for entity {}", system.name, index);
                println!("BINDINGS:   {:?}", self.bindings);
                println!("EQUALITIES: {:?}", self.equalities);
                println!("==============================================");
            }
        }
    }

    pub fn interpret(&mut self, world: &World, system: &SystemSignature) {
        self.bindings.clear();
        self.equalities.clear();

        match system.event.clone() {
            Some(event) => {
                let event_descriptor = world.registered_events.get(&event.name).unwrap().clone();
                let event_instance_size = event_descriptor.size as usize;
                let storage_bitmaps = world.storage_bitmaps.lock().unwrap();
                let event_bitmap = storage_bitmaps.get(&event.name).unwrap();

                for index in 0..MAX_ENTRIES_PER_STORAGE {
                    if event_bitmap.get(index as usize) {
                        if event.args.len() != event_descriptor.members.len() {
                            println!("[System {}] Event {} has {} parameters, but only {} arguments found",
                                     system.name, event.name, event_descriptor.members.len(), event.args.len());
                            return;
                        }

                        for (arg, (id, _)) in event.args.iter().zip(event_descriptor.members.iter()) {
                            if arg.name == "_" { continue; }
                            self.bindings.insert(arg.name.clone(), (event_descriptor.name.clone(), id.clone()));

                            self.interpret_with(world, system);
                        }
                    }
                }
            }

            None => {
                self.interpret_with(world, system);
            }
        }
    }

    pub fn interpret_block(&mut self, block: &RascalBlock, components: Bitmap<MAX_ENTRIES_PER_STORAGE>) {
        for index in 0..MAX_ENTRIES_PER_STORAGE {
            if components.get(index) {
                // TODO: figure out a way to have access to world storage from here -- should already be set up

                for statement in block {
                    match statement {
                        RascalStatement::Assign(a, b) => { /* memory reach */ }
                        RascalStatement::IfElse(_, _, _) => {}
                        RascalStatement::Spawn(_, _) => {}
                        RascalStatement::Destroy(_) => {}
                        RascalStatement::Update(_, _) => {}
                        RascalStatement::Trigger(_) => {}
                    }
                }
            }
        }
    }
}