use std::collections::HashMap;
use std::ops::{BitAnd, Not};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use bitmaps::*;
use lazy_static::lazy_static;

use crate::rascal::parser::{ComponentArgument, ComponentCallSite, ComponentSignature, ComponentType, DataType, Op, RascalBlock, RascalExpression, RascalStatement, Rel, SystemSignature, Un};
use crate::rascal::parser::MemberId;
use crate::rascal::world::ComponentId;
use crate::rascal::world::MAX_ENTRIES_PER_STORAGE;
use crate::rascal::world::World;

lazy_static! {
    static ref STRING_REGISTRY: Mutex<HashMap<String, u32>> = Mutex::new(HashMap::default());
    static ref SYSTEM_QUERY_CACHE: Mutex<HashMap<u64, Bitmap<MAX_ENTRIES_PER_STORAGE>>> = Mutex::new(HashMap::new());
    static ref BINDING_COUNTER: AtomicU64 = AtomicU64::new(1);
    static ref STRING_POOL: Mutex<HashMap<String, u32>> = Mutex::new(HashMap::default());
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum RascalValue {
    Num(i32),
    Bool(bool),
    Text(u32),
    Entity(u64),
    Symbol(u16),
    Point(i32, i32),
    Color(u8, u8, u8),
}

pub fn get_string_index(t: &String) -> u32 {
    let mut pool = STRING_POOL.lock().unwrap();
    if !pool.contains_key(t) {
        let index = pool.len() + 1;
        pool.insert(t.clone(), index as u32);
        index as u32
    } else {
        *pool.get(t).unwrap()
    }
}

pub fn num(v: i32) -> RascalValue { RascalValue::Num(v) }
pub fn bool(v: bool) -> RascalValue { RascalValue::Bool(v) }
pub fn text(v: &str) -> RascalValue {
    RascalValue::Text(get_string_index(&v.to_string()))
}
pub fn entity(v: u64) -> RascalValue { RascalValue::Entity(v) }
pub fn symbol<C: Into<u16>>(c: C) -> RascalValue {
    RascalValue::Symbol(c.into())
}
pub fn point(x: i32, y: i32) -> RascalValue {
    RascalValue::Point(x, y)
}
pub fn color(x: u8, y: u8, z: u8) -> RascalValue {
    RascalValue::Color(x, y, z)
}

impl RascalValue {
    pub fn parse(data_type: DataType, chunk: &[u8]) -> RascalValue {
        match data_type {
            DataType::Num => RascalValue::Num(i32::from_ne_bytes(<[u8; 4]>::try_from(chunk).unwrap())),
            DataType::Bool => RascalValue::Bool(u32::from_ne_bytes(<[u8; 4]>::try_from(chunk).unwrap()) != 0u32),
            DataType::Text => RascalValue::Text(u32::from_ne_bytes(<[u8; 4]>::try_from(chunk).unwrap())),
            DataType::Entity => RascalValue::Entity(u64::from_ne_bytes(<[u8; 8]>::try_from(chunk).unwrap())),
            DataType::Point => RascalValue::Point(
                i32::from_ne_bytes(<[u8; 4]>::try_from(&chunk[0..4]).unwrap()),
                i32::from_ne_bytes(<[u8; 4]>::try_from(&chunk[4..8]).unwrap()),
            ),
            DataType::Symbol => RascalValue::Symbol(u16::from_ne_bytes(<[u8; 2]>::try_from(chunk).unwrap())),
            DataType::Color => RascalValue::Color(chunk[0], chunk[1], chunk[2]),
        }
    }

    pub fn emit(&self) -> Vec<u8> {
        match self {
            RascalValue::Num(num) => Vec::from(num.to_ne_bytes()),
            RascalValue::Bool(status) => Vec::from((if *status { 1u32 } else { 0u32 }).to_ne_bytes()),
            RascalValue::Text(index) => Vec::from(index.to_ne_bytes()),
            RascalValue::Entity(e) => Vec::from(e.to_ne_bytes()),
            RascalValue::Symbol(s) => Vec::from(s.to_ne_bytes()),
            RascalValue::Point(a, b) => {
                let mut result = Vec::new();
                result.extend_from_slice(&a.to_ne_bytes());
                result.extend_from_slice(&b.to_ne_bytes());
                result
            }
            RascalValue::Color(x, y, z) => {
                let mut result = Vec::new();
                result.extend_from_slice(&x.to_ne_bytes());
                result.extend_from_slice(&y.to_ne_bytes());
                result.extend_from_slice(&z.to_ne_bytes());
                result
            }
        }
    }
}

#[derive(Debug)]
pub enum EqualityWith {
    Alias(String),
    Value(RascalExpression),
}

#[derive(Debug)]
pub struct RascalVM {
    pub bindings: HashMap<String, (ComponentId, MemberId)>,
    pub equalities: HashMap<String, EqualityWith>,
    pub push_values: Vec<String>,
}

impl RascalVM {
    pub fn new() -> Self {
        RascalVM {
            bindings: HashMap::default(),
            equalities: HashMap::default(),
            push_values: Vec::default(),
        }
    }

    pub fn query_storages(&mut self, world: &World, comps: &Vec<ComponentCallSite>) -> Bitmap<MAX_ENTRIES_PER_STORAGE> {
        let empty_map = Bitmap::<MAX_ENTRIES_PER_STORAGE>::new().not();
        comps.iter().fold(empty_map, |b, c| {
            assert!(c.is_owned);
            let name = c.name.clone();

            let bitmap = world.storage_bitmaps.get(&name).unwrap();
            if c.not {
                bitmap.not().bitand(b)
            } else {
                bitmap.bitand(b)
            }
        })
    }

    pub fn check_latest_equality(&self, system_name: &String, world: &World, eq: (String, EqualityWith)) -> Result<(), (u8, u8)> {
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
        if arg.name == "_" && arg.value.is_some() {
            let new_id = format!("${}_{}", arg.name, BINDING_COUNTER.fetch_add(1, Ordering::SeqCst));
            self.equalities.insert(new_id, EqualityWith::Value(arg.value.clone().unwrap()));
        } else if arg.name != "_" && self.bindings.contains_key(&arg.name) {
            let new_id = format!("${}_{}", arg.name, BINDING_COUNTER.fetch_add(1, Ordering::SeqCst));
            self.bindings.insert(new_id.clone(), (comp_signature.name.clone(), id.clone()));
            self.equalities.insert(new_id.clone(), EqualityWith::Alias(arg.name.clone()));

            if self.check_latest_equality(&system_name, world, (new_id, EqualityWith::Alias(arg.name.clone()))).is_err() {
                println!("[System {}] Quitting due to equality errors.", system_name);
                return;
            }
        } else if arg.name != "_" {
            self.bindings.insert(arg.name.clone(), (comp_signature.name.clone(), id.clone()));
            if arg.value.is_some() {
                self.equalities.insert(arg.name.clone(), EqualityWith::Value(arg.value.clone().unwrap()));
            }
        }

        if world.component_types.get(&comp_signature.name) == Some(&ComponentType::Event) {
            if arg.name != "_" {
                self.push_values.push(arg.name.clone());
            }
        }
    }

    pub fn interpret_with(&mut self, world: &World, system: &SystemSignature) {
        println!("{:=^46}", format!(" SYSTEM [{}] ", system.name));

        /* CHECK FOR MULTIPLE OWNERS */ {
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
            if query_bitmap.get(index as usize) {
                let values = self.get_binding_values(world, index as usize);

                for (id, value) in &values {
                    if self.equalities.contains_key(id) {
                        if let EqualityWith::Value(expr) = self.equalities.get(id).unwrap() {
                            if let Some(expr_resolved) = self.interpret_expression(expr, &values) {
                                if *value != expr_resolved {
                                    println!("Value {:?} and pushed value {:?} not equal.", value, expr_resolved);
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
                                println!("Value {:?} and aliased value {:?} not equal.", lhs, rhs);
                                continue 'entries;
                            }
                        },

                        _ => {}
                    }
                }

                println!("[{}] {}!", system.name, index);
            }
        }
    }

    fn get_binding_values(&self, world: &World, index: usize) -> HashMap<String, RascalValue>{
        let mut result = HashMap::new();

        for (name, (component, member)) in &self.bindings {
            let comp_signature = match world.component_types.get(component).unwrap() {
                ComponentType::State => world.registered_states.get(component).unwrap(),
                ComponentType::Event => world.registered_events.get(component).unwrap(),
                _ => unreachable!(),
            };

            let member_signature = comp_signature.members.get(member).unwrap();
            let start_address = comp_signature.size as usize * index + member_signature.offset as usize;
            let end_address = start_address + member_signature.typ.size_in_bytes() as usize;
            let chunk = &world.storage_pointers.get(component).unwrap()[start_address..end_address];
            result.insert(name.clone(), RascalValue::parse(member_signature.typ, chunk));
        }

        result
    }

    pub fn interpret(&mut self, world: &World, system: &SystemSignature) {
        BINDING_COUNTER.store(1, Ordering::SeqCst);

        self.bindings.clear();
        self.equalities.clear();

        match system.event.clone() {
            Some(event) => {
                let event_descriptor = world.registered_events.get(&event.name).unwrap().clone();
                let event_bitmap = world.storage_bitmaps.get(&event.name).unwrap();

                for index in 0..MAX_ENTRIES_PER_STORAGE {
                    if event_bitmap.get(index as usize) {
                        if event.args.len() != event_descriptor.members.len() {
                            println!("[System {}] Event {} has {} parameters, but only {} arguments found",
                                     system.name, event.name, event_descriptor.members.len(), event.args.len());
                            return;
                        }

                        let members_in_order: Vec<_> = event_descriptor.ptrs.iter().map(|(_, mid)| mid).collect();
                        let zipped_arg_and_ids: Vec<_> = event.args.iter().zip(members_in_order).collect();
                        for (arg, id) in zipped_arg_and_ids {
                            self.update_equality(&system.name, world, arg, id, event_descriptor);
                        }

                        self.interpret_with(world, system);
                    }
                }
            }

            None => {
                self.interpret_with(world, system);
            }
        }
    }

    pub fn interpret_expression(&self, expr: &RascalExpression, values: &HashMap<String, RascalValue>) -> Option<RascalValue> {
        match expr {
            RascalExpression::Relation(a, op, b) => {
                let ia = self.interpret_expression(a.as_ref(), values).unwrap();
                let ib = self.interpret_expression(b.as_ref(), values).unwrap();

                use RascalValue::*;
                match (ia, op, ib) {
                    (Num(l), Rel::Eq, Num(r)) => Some(Bool(l == r)),
                    (Num(l), Rel::Ne, Num(r)) => Some(Bool(l != r)),
                    (Num(l), Rel::Le, Num(r)) => Some(Bool(l <= r)),
                    (Num(l), Rel::Lt, Num(r)) => Some(Bool(l < r)),
                    (Num(l), Rel::Ge, Num(r)) => Some(Bool(l >= r)),
                    (Num(l), Rel::Gt, Num(r)) => Some(Bool(l > r)),
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

            RascalExpression::Point(a, b) => {
                use RascalValue::*;
                let ia = self.interpret_expression(a.as_ref(), values);
                let ib = self.interpret_expression(b.as_ref(), values);
                match (ia, ib) {
                    (Some(Num(x)), Some(Num(y))) => Some(RascalValue::Point(x, y)),
                    _ => None
                }
            }

            RascalExpression::Color(x, y, z) => {
                use RascalValue::*;
                let ia = self.interpret_expression(x.as_ref(), values);
                let ib = self.interpret_expression(y.as_ref(), values);
                let ic = self.interpret_expression(z.as_ref(), values);
                match (ia, ib, ic) {
                    (Some(Num(x)), Some(Num(y)), Some(Num(z))) => Some(RascalValue::Color(x as u8, y as u8, z as u8)),
                    _ => None
                }
            }

            RascalExpression::BoolLiteral(b) => Some(RascalValue::Bool(*b)),
            RascalExpression::NumLiteral(n) => Some(RascalValue::Num(*n)),
            RascalExpression::SymbolLiteral(s) => Some(RascalValue::Symbol(*s)),
            RascalExpression::TextLiteral(t) => {
                let string_pool_index = get_string_index(t);
                Some(RascalValue::Text(string_pool_index))
            },
            RascalExpression::Identifier(ident) if values.contains_key(ident) => {
                Some(values.get(ident).unwrap().clone())
            }
            _ => None
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