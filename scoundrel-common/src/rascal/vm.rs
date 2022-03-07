use std::collections::HashMap;
use std::ops::{BitAnd, Not};
use std::ptr::copy_nonoverlapping;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use bitmaps::*;
use lazy_static::lazy_static;

use crate::colors::Color;
use crate::engine::SCREEN;
use crate::rascal::parser::{ComponentArgument, ComponentCallSite, ComponentSignature, ComponentType, DataType, Op, RascalBlock, RascalExpression, RascalStatement, Rel, SystemSignature, Un};
use crate::rascal::parser::MemberId;
use crate::rascal::world::{AddComponent, ComponentId};
use crate::rascal::world::EntityId;
use crate::rascal::world::MAX_ENTRIES_PER_STORAGE;
use crate::rascal::world::World;

lazy_static! {
    static ref STRING_REGISTRY: Mutex<HashMap<String, u32>> = Mutex::new(HashMap::default());
    static ref SYSTEM_QUERY_CACHE: Mutex<HashMap<u64, Bitmap<MAX_ENTRIES_PER_STORAGE>>> = Mutex::new(HashMap::new());
    static ref BINDING_COUNTER: AtomicU64 = AtomicU64::new(1);
    static ref STRING_POOL: Mutex<HashMap<String, u32>> = Mutex::new(HashMap::default());
    static ref STRING_REPOOL: Mutex<HashMap<u32, String>> = Mutex::new(HashMap::default());
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum RascalValue {
    Num(i32),
    Bool(bool),
    Text(u32),
    Entity(usize),
    Symbol(u16),
}

pub fn retrieve_string(index: u32) -> String {
    let mut pool = STRING_REPOOL.lock().unwrap();
    pool.get(&index).unwrap().clone()
}

pub fn get_or_insert_into_string_pool(t: &String) -> u32 {
    let mut pool = STRING_POOL.lock().unwrap();
    let index = if !pool.contains_key(t) {
        let index = pool.len() + 1;
        pool.insert(t.clone(), index as u32);
        STRING_REPOOL.lock().unwrap().insert(index as u32, t.clone());
        index as u32
    } else {
        *pool.get(t).unwrap()
    };

    index
}

pub fn num(v: i32) -> RascalValue { RascalValue::Num(v) }
pub fn bool(v: bool) -> RascalValue { RascalValue::Bool(v) }
pub fn text(v: &str) -> RascalValue {
    RascalValue::Text(get_or_insert_into_string_pool(&v.to_string()))
}
pub fn entity(v: usize) -> RascalValue { RascalValue::Entity(v) }
pub fn symbol<C: Into<u16>>(c: C) -> RascalValue {
    RascalValue::Symbol(c.into())
}

impl RascalValue {
    pub fn parse(data_type: DataType, chunk: &[u8]) -> RascalValue {
        match data_type {
            DataType::Num => RascalValue::Num(i32::from_ne_bytes(<[u8; 4]>::try_from(chunk).unwrap())),
            DataType::Bool => RascalValue::Bool(u32::from_ne_bytes(<[u8; 4]>::try_from(chunk).unwrap()) != 0u32),
            DataType::Text => {
                let index = u32::from_ne_bytes(<[u8; 4]>::try_from(chunk).unwrap());
                RascalValue::Text(index)
            },
            DataType::Entity => RascalValue::Entity(usize::from_ne_bytes(<[u8; 8]>::try_from(chunk).unwrap())),
            DataType::Symbol => RascalValue::Symbol(u16::from_ne_bytes(<[u8; 2]>::try_from(chunk).unwrap())),
        }
    }

    pub fn emit(&self) -> Vec<u8> {
        match self {
            RascalValue::Num(num) => Vec::from(num.to_ne_bytes()),
            RascalValue::Bool(status) => Vec::from((if *status { 1u32 } else { 0u32 }).to_ne_bytes()),
            RascalValue::Text(index) => Vec::from(index.to_ne_bytes()),
            RascalValue::Entity(e) => Vec::from(e.to_ne_bytes()),
            RascalValue::Symbol(s) => Vec::from(s.to_ne_bytes()),
        }
    }
}

#[derive(Debug)]
pub enum SemanticChange {
    ValueAssign(String, RascalValue),
    ValueInc(String, RascalValue),
    ValueDec(String, RascalValue),
    SpawnEntity(String, Vec<ComponentCallSite>),
    DestroyEntity(String),
    UpdateComponents(String, Vec<ComponentCallSite>),
    TriggerEvent(ComponentCallSite),
    ConsumeEvent,
    Print(RascalExpression),
}

#[derive(Debug)]
pub enum EqualityWith {
    Alias(String),
    Value(RascalExpression),
}

type Offset = usize;

#[derive(Debug)]
pub struct RascalVM {
    current_system: Option<SystemSignature>,
    current_event_handled: Option<usize>,
    pub bindings: HashMap<String, (ComponentId, MemberId)>,
    pub equalities: HashMap<String, EqualityWith>,
    pub push_values: Vec<String>,
}

impl RascalVM {
    pub fn new() -> Self {
        RascalVM {
            current_system: None,
            current_event_handled: None,
            bindings: HashMap::default(),
            equalities: HashMap::default(),
            push_values: Vec::default(),
        }
    }

    fn query_storages(&mut self, world: &World, comps: &Vec<ComponentCallSite>) -> Bitmap<MAX_ENTRIES_PER_STORAGE> {
        let empty_map = Bitmap::<MAX_ENTRIES_PER_STORAGE>::new().not();
        comps.iter().fold(empty_map, |b, c| {
            assert!(c.is_owned);
            let name = c.name.clone();

            let bitmap = world.storage_bitmaps.get(&name).unwrap();
            println!("{}? {} {:?}", c.name.clone(), c.not, bitmap);
            if c.not {
                bitmap.not().bitand(b)
            } else {
                bitmap.bitand(b)
            }
        })
    }

    fn check_latest_equality(&self, system_name: &String, world: &World, eq: (String, EqualityWith)) -> Result<(), (u8, u8)> {
        fn get_size(world: &World, comp: &ComponentId, member: &MemberId) -> u8 {
            match world.component_types.get(comp).unwrap() {
                ComponentType::State =>
                    world.registered_states.get(comp).unwrap().members[member].typ.size_in_bytes(),
                ComponentType::Event =>
                    world.registered_events.get(comp).unwrap().members[member].typ.size_in_bytes(),
                ComponentType::Tag => 0u8,
                other => {
                    println!("{:?}", other);
                    unreachable!()
                }
            }
        }

        if let (lhs, EqualityWith::Alias(rhs)) = eq {
            let (left_comp, left_member) = self.bindings.get(&lhs).unwrap();
            let (right_comp, right_member) = self.bindings.get(&rhs).unwrap();

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


    fn update_equality(&mut self, system_name: &String, world: &World, arg: &ComponentArgument, id: &MemberId, comp_signature: &ComponentSignature) {
        if arg.name != "_" {
            self.bindings.insert(arg.name.clone(), (comp_signature.name.clone(), id.clone()));

            if arg.value.is_some() {
                self.equalities.insert(arg.name.clone(), EqualityWith::Value(arg.value.clone().unwrap()));
            }
        } else {
            let new_id = format!("${}_{}", arg.name, BINDING_COUNTER.fetch_add(1, Ordering::SeqCst));
            self.bindings.insert(new_id.clone(), (comp_signature.name.clone(), id.clone()));

            let is_alias = arg.name != "_" && self.bindings.contains_key(&arg.name);

            self.equalities.insert(new_id.clone(),
                                   if is_alias { EqualityWith::Alias(arg.name.clone()) }
                                   else { EqualityWith::Value(arg.value.clone().unwrap()) });

            if is_alias {
                if self.check_latest_equality(&system_name, world, (new_id, EqualityWith::Alias(arg.name.clone()))).is_err() {
                    println!("[System {}] Quitting due to equality errors.", system_name);
                    return;
                }
            }
        }

        if world.component_types.get(&comp_signature.name) == Some(&ComponentType::Event) {
            if arg.name != "_" {
                self.push_values.push(arg.name.clone());
            }
        }
    }

    fn get_binding_values(&self, world: &World, index: usize) -> HashMap<String, RascalValue>{
        let mut result = HashMap::new();

        for (name, (component, member)) in &self.bindings {
            let (comp_signature, index) = match world.component_types.get(component).unwrap() {
                ComponentType::State => (world.registered_states.get(component).unwrap(), index),
                ComponentType::Event => (world.registered_events.get(component).unwrap(), self.current_event_handled.unwrap()),
                _ => unreachable!(),
            };

            let member_signature = comp_signature.members.get(member).unwrap();
            let member_size = member_signature.typ.size_in_bytes() as usize;

            let start_address = comp_signature.size as usize * index + member_signature.offset as usize;
            let end_address = start_address + member_size;
            let chunk = &world.storage_pointers.get(component).unwrap()[start_address..end_address];

            let value_from_chunk = RascalValue::parse(member_signature.typ, chunk);
            result.insert(name.clone(), value_from_chunk);
        }

        result
    }

    fn interpret_with(&mut self, world: &mut World, system: &SystemSignature) {
        let owner = /* CHECK FOR MULTIPLE OWNERS */ {
            let with = system.with.clone();
            let mut owners: Vec<_> = with.iter().map(|o| o.owner.clone().unwrap_or(String::default())).collect();
            owners.sort();
            owners.dedup();
            if owners.len() > 1 {
                println!("ERROR [{}]: more than one owning entity per system is not supported yet. Qutting.", system.name);
                println!("DEBUG: owners = {:?}", owners);
                return;
            }

            owners.first().map(|f| f.clone())
        };

        println!("----------- {}", system.name.clone());
        let query_bitmap = self.query_storages(world, &system.with);
        println!("-----------------------------------");

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
                        self.update_equality(&system.name, world, arg, id, comp_signature);
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

        'entries: for index in 0..MAX_ENTRIES_PER_STORAGE {
            if !world.entity_bitmaps.get(index as usize) { continue; }

            if query_bitmap.get(index as usize) {
                let mut values = self.get_binding_values(world, index as usize);
                for (id, value) in &values {
                    if self.equalities.contains_key(id) {
                        if let EqualityWith::Value(expr) = self.equalities.get(id).unwrap() {
                            if let Some(expr_resolved) = self.interpret_expression(expr, &values) {
                                if *value != expr_resolved {
                                    continue 'entries;
                                }
                            }
                        }
                    }
                }

                for eq in &self.equalities {
                    match eq {
                        (name, EqualityWith::Alias(real)) => {
                            let lhs = values.get(name).unwrap();
                            let rhs = values.get(real).unwrap();
                            if *lhs != *rhs {
                                continue 'entries;
                            }
                        },

                        _ => {}
                    }
                }

                match &owner {
                    None => values.insert("@".to_string(), RascalValue::Entity(index)),
                    Some(name) if name.trim() == "" => values.insert("@".to_string(), RascalValue::Entity(index)),
                    Some(name) => values.insert(name.clone(), RascalValue::Entity(index)),
                };

                let changes = self.interpret_block(&system.body, &values);
                self.commit_changes(world, index, &mut values, changes);
            }
        }
    }

    fn interpret_expression(&self, expr: &RascalExpression, values: &HashMap<String, RascalValue>) -> Option<RascalValue> {

        match expr {
            RascalExpression::Relation(a, op, b) => {
                let ia = self.interpret_expression(a.as_ref(), values).unwrap();
                let ib = self.interpret_expression(b.as_ref(), values).unwrap();

                use RascalValue::*;
                match (ia, op, ib) {
                    (Num(l), Rel::Le, Num(r)) => Some(Bool(l <= r)),
                    (Num(l), Rel::Lt, Num(r)) => Some(Bool(l < r)),
                    (Num(l), Rel::Ge, Num(r)) => Some(Bool(l >= r)),
                    (Num(l), Rel::Gt, Num(r)) => Some(Bool(l > r)),
                    (lhs, Rel::Eq, rhs) => Some(Bool(lhs == rhs)),
                    (lhs, Rel::Ne, rhs) => Some(Bool(lhs != rhs)),
                    _ => None
                }
            }

            RascalExpression::Binary(a, op, b) => {
                let ia = self.interpret_expression(a.as_ref(), values).unwrap();
                let ib = self.interpret_expression(b.as_ref(), values).unwrap();

                use RascalValue::*;
                match (ia, op, ib) {
                    (Num(l), Op::Add, Num(r)) => Some(Num(l + r)),
                    (Num(l), Op::Sub, Num(r)) => Some(Num(l - r)),
                    (Num(l), Op::Mul, Num(r)) => Some(Num(l * r)),
                    (Num(l), Op::Div, Num(r)) => Some(Num(l / r)),
                    (Num(l), Op::Mod, Num(r)) => Some(Num(l % r)),
                    (Text(l), Op::Add, rhs) => {
                        let string = retrieve_string(l);
                        let right = match rhs {
                            RascalValue::Num(v) => v.to_string(),
                            RascalValue::Bool(b) => b.to_string(),
                            RascalValue::Text(t) => retrieve_string(t),
                            RascalValue::Symbol(c) => c.to_string(),
                            RascalValue::Entity(u) => format!("#{}", u),
                        };
                        let new_index = get_or_insert_into_string_pool(&format!("{}{}", string, right));
                        Some(Text(new_index))
                    }
                    _ => None
                }
            }

            RascalExpression::Unary(Un::Not, a) => {
                let ia = self.interpret_expression(a.as_ref(), values).unwrap();

                use RascalValue::*;
                match ia {
                    Bool(b) => Some(Bool(!b)),
                    _ => None
                }
            }

            RascalExpression::Unary(Un::Neg, a) => {
                let ia = self.interpret_expression(a.as_ref(), values).unwrap();

                use RascalValue::*;
                match ia {
                    Num(b) => Some(Num(-b)),
                    _ => None
                }
            }

            RascalExpression::Bool(b) => self.interpret_expression(b.as_ref(), values),
            RascalExpression::Num(n) => self.interpret_expression(n.as_ref(), values),
            RascalExpression::Symbol(s) => self.interpret_expression(s.as_ref(), values),

            RascalExpression::BoolLiteral(b) => Some(RascalValue::Bool(*b)),
            RascalExpression::NumLiteral(n) => Some(RascalValue::Num(*n)),
            RascalExpression::SymbolLiteral(s) => Some(RascalValue::Symbol(*s)),
            RascalExpression::TextLiteral(t) => {
                let string_pool_index = get_or_insert_into_string_pool(t);
                Some(RascalValue::Text(string_pool_index))
            },
            RascalExpression::Identifier(ident) if values.contains_key(ident) => {
                Some(values.get(ident).unwrap().clone())
            }
            other => {
                println!("[parse_expression] failed to catch: {:?}", other);
                None
            }
        }
    }

    fn interpret_statement(&self, statement: &RascalStatement, values: &HashMap<String, RascalValue>) -> Vec<SemanticChange> {
        let mut result = Vec::new();

        match statement {
            RascalStatement::Assign(a, b) => {
                if let RascalExpression::Identifier(name) = a {
                    if !values.contains_key(name) {
                        println!("Variable {} not found", name);
                        return vec![];
                    }

                    result.push(SemanticChange::ValueAssign(name.clone(), self.interpret_expression(b, values).unwrap()));
                } else {
                    println!("Expected identifier, got {:?}", a);
                    return vec![];
                }
            }

            RascalStatement::IfElse(body, then, or_else) => {
                match self.interpret_expression(body, values) {
                    Some(RascalValue::Bool(is)) => {
                        if is {
                            result.extend(self.interpret_block(then.as_ref(), values));
                        } else if or_else.is_some() {
                            result.extend(self.interpret_block(or_else.as_ref().unwrap(), values));
                        }
                    }

                    Some(other) => {
                        println!("If guard didn't evaluate to boolean, but rather to: {:?}!", other);
                        unreachable!()
                    }

                    None => {
                        println!("It was impossible to interpret the evaluated expression: {:?}\n\t{:?}", body, values);
                        unreachable!()
                    }
                }
            }

            RascalStatement::Spawn(a, b) => {
                if let RascalExpression::Identifier(name) = a {
                    result.push(SemanticChange::SpawnEntity(name.clone(), b.clone()));
                } else {
                    println!("Expected identifier, got {:?}", a);
                    return vec![];
                }
            }

            RascalStatement::Destroy(id) => {
                if let RascalExpression::Identifier(name) = id {
                    result.push(SemanticChange::DestroyEntity(name.clone()));
                } else {
                    println!("Expected identifier, got {:?}", id);
                    return vec![];
                }
            }

            RascalStatement::Update(x, y) => {
                if let RascalExpression::Identifier(name) = x {
                    result.push(SemanticChange::UpdateComponents(name.clone(), y.clone()));
                } else {
                    println!("Expected identifier, got {:?}", x);
                    return vec![];
                }
            }

            RascalStatement::Trigger(e) => {
                result.push(SemanticChange::TriggerEvent(e.clone()));
            }

            RascalStatement::ConsumeEvent => {
                let system = self.current_system.clone().unwrap();
                if system.event.is_none() {
                    println!("Trying to consume event from a stateful system. Ignoring.");
                } else {
                    result.push(SemanticChange::ConsumeEvent);
                }
            }

            RascalStatement::Print(e) => {
                result.push(SemanticChange::Print(e.clone()));
            }

            RascalStatement::Match(key, cases) => {
                match self.interpret_expression(key, values) {
                    Some(value) => {
                        for (case_match, case_body) in cases {
                            let other = self.interpret_expression(case_match, values).unwrap();
                            if value == other {
                                result.extend(self.interpret_block(case_body, values));
                                break;
                            }
                        }
                    }

                    None => {
                        println!("It was impossible to interpret the key of the match: {:?}\n\t{:?}", key, values);
                        unreachable!()
                    }
                }
            }

            RascalStatement::Inc(var, n) => {
                if let RascalExpression::Identifier(name) = var {
                    if !values.contains_key(name) {
                        println!("Variable {} not found", name);
                        return vec![];
                    }

                    result.push(SemanticChange::ValueInc(name.clone(), self.interpret_expression(n, values).unwrap()));
                } else {
                    println!("Expected identifier, got {:?}", var);
                    return vec![];
                }
            }

            RascalStatement::Dec(var, n) => {
                if let RascalExpression::Identifier(name) = var {
                    if !values.contains_key(name) {
                        println!("Variable {} not found", name);
                        return vec![];
                    }

                    result.push(SemanticChange::ValueDec(name.clone(), self.interpret_expression(n, values).unwrap()));
                } else {
                    println!("Expected identifier, got {:?}", var);
                    return vec![];
                }
            }

        };

        result
    }

    fn interpret_block(&self, block: &RascalBlock, values: &HashMap<String, RascalValue>) -> Vec<SemanticChange> {
        let mut result = Vec::new();

        for statement in block {
            result.extend(self.interpret_statement(statement, values));
        }

        result
    }

    fn add_component(&self, world: &mut World, entity: EntityId, comp: ComponentCallSite, values: &HashMap<String, RascalValue>) {
        let comp_type = world.component_types.get(&comp.name).unwrap();
        match comp_type {
            ComponentType::State => {
                let mut args = Vec::new();

                for arg in comp.args {
                    if arg.value.is_none() {
                        println!("State components need to have values.");
                        return;
                    } else {
                        args.extend(self.interpret_expression(&arg.value.unwrap(), values).unwrap().emit());
                    }
                }

                world.add_component(entity, comp.name.as_str(), args);
            }
            ComponentType::Tag => {
                if !comp.not {
                    world.add_tag(entity, comp.name.as_str());
                }
            }

            _ => {
                println!("Events and systems cannot be added to spawned entities.");
            }
        }
    }

    fn assign_value(&mut self,  world: &mut World, index: usize, name: String, new_value: RascalValue) {
        let (comp_id, member_id) = self.bindings.get(&name).unwrap();
        if let ComponentType::State = world.component_types.get(comp_id).unwrap() {
            let descriptor = world.registered_states.get(comp_id).unwrap();
            let instance_size = descriptor.size as usize;

            let ptr_start = instance_size * index;
            let member_hash: HashMap<_, _> = descriptor.ptrs.iter().map(|(a, b)| (b.clone(), *a)).collect();
            let ptr_offset = *member_hash.get(member_id).unwrap();
            let member_start = ptr_start + ptr_offset as usize;
            let size = descriptor.members.get(member_id).unwrap().typ.size_in_bytes() as usize;

            let binary_value = new_value.emit();
            assert_eq!(binary_value.len(), size);

            unsafe {
                let storage = world.storage_pointers.get_mut(comp_id).unwrap().as_mut_ptr();
                let comp_storage_at_entity_ptr = storage.offset(member_start as isize);
                copy_nonoverlapping(binary_value.as_ptr(), comp_storage_at_entity_ptr, size);
            }
        } else {
            println!("Only states are mutable ({} is not writable).", name);
        }
    }

    fn commit_changes(&mut self, world: &mut World, index: usize, values: &mut HashMap<String, RascalValue>, changes: Vec<SemanticChange>) {
        for change in changes {
            match change {

                SemanticChange::ValueInc(name, mod_value) => {
                    if let RascalValue::Num(mod_num) = mod_value {
                        match values.get(&name).unwrap() {
                            RascalValue::Num(num) => {
                                let new_value = RascalValue::Num(*num + mod_num);
                                values.insert(name.clone(), new_value.clone());
                                self.assign_value(world, index, name, new_value);
                            }

                            _ => unreachable!()
                        }
                    }
                }

                SemanticChange::ValueDec(name, mod_value) => {
                    if let RascalValue::Num(num) = values.get(&name).unwrap() {
                        if let RascalValue::Num(mod_num) = mod_value {
                            let new_value = RascalValue::Num(*num - mod_num);
                            values.insert(name.clone(), new_value.clone());
                            self.assign_value(world, index, name, new_value);
                        }
                    }
                }

                SemanticChange::ValueAssign(name, new_value) => {
                    values.insert(name.clone(), new_value.clone());
                    self.assign_value(world, index, name, new_value);
                }

                SemanticChange::SpawnEntity(name, comps) => {
                    let entity = world.create_entity();
                    for comp in comps {
                        self.add_component(world, entity, comp, values);
                    }
                    values.insert(name, RascalValue::Entity(entity));
                }

                SemanticChange::DestroyEntity(entity) => {
                    if let RascalValue::Entity(index) = values.get(&entity).unwrap() {
                        let index = *index as usize;
                        world.entity_bitmaps.set(index, false);

                        for comp in &world.component_names {
                            let mut bitmap = world.storage_bitmaps.get_mut(comp).unwrap();
                            bitmap.set(index, false);
                        }
                    }
                }

                SemanticChange::UpdateComponents(name, comps) => {
                    println!("{:?}", values);
                    if let Some(RascalValue::Entity(index)) = values.get(&name) {
                        let index = *index;
                        if world.entity_bitmaps.get(index) {
                            for comp in comps {
                                if comp.not {
                                    world.storage_bitmaps.get_mut(&comp.name).unwrap().set(index, false);
                                } else {
                                    self.add_component(world, index, comp, values);
                                }
                            }
                        }
                    }
                }

                SemanticChange::TriggerEvent(event) => {
                    let entity = world.create_entity();
                    self.add_component(world, entity, event, values);
                }

                SemanticChange::ConsumeEvent => {
                    if let Some(system) = &self.current_system {
                        let index = self.current_event_handled.unwrap();
                        world.storage_bitmaps.get_mut(&system.event.as_ref().unwrap().name).unwrap().set(index, false);
                    }
                }

                SemanticChange::Print(expr) => {
                    let x = 0;
                    let y = 0;
                    let v = self.interpret_expression(&expr, &values).unwrap();
                    let text = if let RascalValue::Text(val) = v {
                        retrieve_string(val)
                    } else {
                        format!("{:?}", v)
                    };

                    let mut screen = SCREEN.write().unwrap();
                    if screen.is_ready() {
                        let glyphs = screen.glyphs_mut();
                        let mut index = 0;
                        for c in text.chars() {
                            glyphs[index].symbol = c as u32;
                            glyphs[index].foreground = Color::new(255, 255, 255);
                            glyphs[index].background = Color::new(0, 0, 0);
                            index += 1;
                        }
                    }
                }
            }
        }
    }

    pub fn interpret(&mut self, world: &mut World, system: &SystemSignature) {
        BINDING_COUNTER.store(1, Ordering::SeqCst);

        self.current_system = Some(system.clone());

        match system.event.clone() {
            Some(event) => {
                let event_bitmap = world.storage_bitmaps.get(&event.name).unwrap().clone();

                for index in 0..MAX_ENTRIES_PER_STORAGE {
                    if !world.entity_bitmaps.get(index as usize) { continue; }

                    if event_bitmap.get(index as usize) {
                        self.bindings.clear();
                        self.equalities.clear();
                        self.current_event_handled = Some(index as usize);
                        let event_descriptor = world.registered_events.get(&event.name).unwrap().clone();
                        let members_len = event_descriptor.members.len();

                        if event.args.len() != members_len {
                            println!("[System {}] Event {} has {} parameters, but only {} arguments found",
                                     system.name, event.name, event_descriptor.members.len(), event.args.len());
                            return;
                        }

                        let members_in_order: Vec<_> = event_descriptor.ptrs.iter().map(|(_, mid)| mid.clone()).collect();
                        let zipped_arg_and_ids: Vec<_> = event.args.iter().zip(members_in_order).collect();
                        for (arg, id) in zipped_arg_and_ids {
                            self.update_equality(&system.name, world, arg, &id, &event_descriptor);
                        }

                        self.interpret_with(world, system);
                        self.current_event_handled = None;
                    }
                }
            }

            None => {
                self.bindings.clear();
                self.equalities.clear();
                self.interpret_with(world, system);
            }
        }

        self.current_system = None;
    }
}