use std::collections::HashMap;
use std::ops::{BitAnd, Not};
use std::ptr::copy_nonoverlapping;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use bitmaps::*;
use lazy_static::lazy_static;

use crate::colors::Color;
use crate::rascal::parser::{BoolOper, ComponentCallSite, ComponentModifier, ComponentSignature, ComponentType, DataType, Op, RascalBlock, RascalExpression, RascalGlyphColor, RascalGlyphPosition, RascalStatement, Rel, SystemSignature, Un};
use crate::rascal::parser::MemberId;
use crate::rascal::semantics::{Position, SemanticChange};
use crate::rascal::world::{AddComponent, BinaryComponent, ComponentId, FieldId, TriggerEvent};
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
    Field(FieldId),
}

impl RascalValue {
    pub fn get_datatype(&self) -> DataType {
        match self {
            RascalValue::Num(_) => DataType::Num,
            RascalValue::Bool(_) => DataType::Bool,
            RascalValue::Text(_) => DataType::Text,
            RascalValue::Entity(_) => DataType::Entity,
            RascalValue::Symbol(_) => DataType::Symbol,
            RascalValue::Field(_) => DataType::Field,
        }
    }

    pub fn same_optic(a: RascalValue, b: RascalValue) -> bool {
        use RascalValue::*;
        match (a, b) {
            (Num(x), Entity(y)) if (x as usize) == y => true,
            (Entity(x), Num(y)) if (y as usize) == x => true,
            (x, y) => x == y
        }
    }

    pub fn as_num(&self) -> Option<i32> {
        if let RascalValue::Num(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

pub fn retrieve_string(index: u32) -> String {
    let mut pool = STRING_REPOOL.lock().unwrap();
    pool.get(&index).unwrap().clone()
}

pub fn rascal_value_as_string(v: RascalValue) -> String {
    match v {
        RascalValue::Text(val) => retrieve_string(val),
        RascalValue::Symbol(glyph) => {
            format!("{}", glyph as u8 as char)
        },
        _ => format!("{:?}", v),
    }
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
            DataType::Field => RascalValue::Field(u32::from_ne_bytes(<[u8; 4]>::try_from(chunk).unwrap())),
        }
    }

    pub fn emit(&self) -> Vec<u8> {
        match self {
            RascalValue::Num(num) => Vec::from(num.to_ne_bytes()),
            RascalValue::Bool(status) => Vec::from((if *status { 1u32 } else { 0u32 }).to_ne_bytes()),
            RascalValue::Text(index) => Vec::from(index.to_ne_bytes()),
            RascalValue::Entity(e) => Vec::from(e.to_ne_bytes()),
            RascalValue::Symbol(s) => Vec::from(s.to_ne_bytes()),
            RascalValue::Field(f) => Vec::from(f.to_ne_bytes()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EqualityWith {
    Alias(String),
    Value(RascalExpression),
}

type Offset = usize;

#[derive(Debug)]
pub struct RascalVM {
    pub(crate) current_system: Option<SystemSignature>,
    pub(crate) current_index: usize,
    pub(crate) current_event: Option<(ComponentSignature, BinaryComponent)>,
    pub(crate) event_consumed: bool,
    pub(crate) something_printed: bool,
    pub bindings: HashMap<String, (ComponentId, MemberId)>,
    pub equalities: HashMap<String, EqualityWith>,
    pub push_values: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum RascalEventfulResult {
    Propagated,
    Consumed,
}

pub enum RascalInterpretResult {
    Ok,
    Err(String)
}

impl RascalVM {
    pub fn new() -> Self {
        RascalVM {
            current_system: None,
            current_event: None,
            current_index: 0,
            event_consumed: false,
            something_printed: false,
            bindings: HashMap::default(),
            equalities: HashMap::default(),
            push_values: Vec::default(),
        }
    }

    pub fn is_something_printed(&self) -> bool {
        self.something_printed
    }

    fn query_storages(&mut self, world: &World, comps: &Vec<ComponentCallSite>) -> Bitmap<MAX_ENTRIES_PER_STORAGE> {
        let empty_map = Bitmap::<MAX_ENTRIES_PER_STORAGE>::new().not();
        comps.iter().fold(empty_map, |b, c| {
            assert!(c.is_owned);
            let name = c.name.clone();

            let bitmap = world.storage_bitmaps.get(&name).unwrap();

            if c.modifier == ComponentModifier::Default {
                bitmap.bitand(b)
            } else {
                bitmap.not().bitand(b)
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


    fn update_equality(&mut self, system_name: &String, world: &World, arg: &Option<RascalExpression>,
                       id: &MemberId, comp_signature: &ComponentSignature) {

        if let Some(RascalExpression::Identifier(var_id)) = arg {
            if var_id != "_" {
                if !self.bindings.contains_key(var_id) {
                    self.bindings.insert(var_id.clone(), (comp_signature.name.clone(), id.clone()));
                } else {
                    let new_id = format!("${}_{}", id, BINDING_COUNTER.fetch_add(1, Ordering::SeqCst));
                    self.bindings.insert(new_id.clone(), (comp_signature.name.clone(), id.clone()));
                    self.equalities.insert(new_id.clone(), EqualityWith::Alias(var_id.to_string()));
                }
            }
        } else if let Some(expr) = arg {
            let new_id = format!("${}_{}", id, BINDING_COUNTER.fetch_add(1, Ordering::SeqCst));
            self.bindings.insert(new_id.clone(), (comp_signature.name.clone(), id.clone()));
            self.equalities.insert(new_id.clone(), EqualityWith::Value(expr.clone()));
        }
    }

    fn get_binding_values(&self, world: &World, index: usize) -> HashMap<String, RascalValue>{
        let mut result = HashMap::new();

        for (name, (component, member)) in &self.bindings {
            let mut is_event = false;
            if *component == "" { continue; }
            let comp_signature = match world.component_types.get(component).unwrap() {
                ComponentType::State => world.registered_states.get(component).unwrap(),
                ComponentType::Event => { is_event = true; world.registered_events.get(component).unwrap() },
                _ => unreachable!(),
            };

            let member_signature = comp_signature.members.get(member).unwrap();
            let member_size = member_signature.typ.size_in_bytes() as usize;

            let start_address = member_signature.offset as usize + if is_event { 0 } else { comp_signature.size as usize * index };
            let end_address = start_address + member_size;
            let chunk = if is_event {
                &self.current_event.as_ref().unwrap().1
            } else {
                &world.storage_pointers.get(component).unwrap()
            };

            let value_from_chunk = RascalValue::parse(member_signature.typ, &chunk[start_address..end_address]);
            result.insert(name.clone(), value_from_chunk);
        }

        result
    }

    fn interpret_with(&mut self, world: &mut World, system: &SystemSignature) {
        let owner = /* CHECK FOR MULTIPLE OWNERS */ {
            let with = system.storage_requirements.clone();
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

        let query_bitmap = self.query_storages(world, &system.storage_requirements);

        for uniq in &system.unique_requirements {
            let index = get_or_insert_into_string_pool(uniq);
            if !world.unique_storage.contains_key(&index) {
                println!("[System {}] Unique value {} required but not found!",
                         system.name, uniq.as_str());
                return;
            }
        }

        for call_site in system.storage_requirements.clone() {
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

        let saved_bindings = self.bindings.clone();
        let saved_equalities = self.equalities.clone();

        'entries: for index in 0..MAX_ENTRIES_PER_STORAGE {
            if query_bitmap.get(index as usize) {
                self.current_index = index;
                self.bindings = saved_bindings.clone();
                self.equalities = saved_equalities.clone();
                let mut values = self.get_binding_values(world, index as usize);

                values.insert("_".to_string(), RascalValue::Entity(index));

                match &owner {
                    None => { values.insert("_".to_string(), RascalValue::Entity(index)); }
                    Some(name) if name.trim() == "" => { values.insert("_".to_string(), RascalValue::Entity(index)); }
                    Some(name) => {
                        if values.contains_key(name) {
                            let cached_value = values.get(name).unwrap();
                            if !RascalValue::same_optic(cached_value.clone(), RascalValue::Entity(index)) {
                                continue 'entries;
                            }
                        }
                        values.insert(name.clone(), RascalValue::Entity(index));
                        self.bindings.insert(name.clone(), ("".to_string(), "<owner>".to_string()));
                        if self.equalities.contains_key(name) {
                            match self.equalities.get(name).unwrap() {
                                EqualityWith::Value(RascalExpression::NumLiteral(x)) if (*x as usize) != index => {
                                    continue 'entries;
                                }
                                _ => {}
                            }
                        }
                        self.equalities.insert(name.clone(), EqualityWith::Value(RascalExpression::NumLiteral(index as i32)));
                    }
                };

                for (id, value) in &values {
                    if self.equalities.contains_key(id) {
                        match self.equalities.get(id) {
                            Some(EqualityWith::Value(expr)) => {
                                if let Some(expr_resolved) = self.interpret_expression(expr, &values, world) {
                                    if !RascalValue::same_optic(value.clone(), expr_resolved.clone()) {
                                        continue 'entries;
                                    }
                                }
                            }

                            Some(EqualityWith::Alias(alias)) => {
                                if let Some(alias_value) = values.get(alias) {
                                    if !RascalValue::same_optic(value.clone(), alias_value.clone()) {
                                        continue 'entries;
                                    }
                                }
                            }

                            None => {}
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

                self.interpret_block(&system.body_block, &mut values, world);
            }
        }
    }

    pub(crate) fn interpret_expression(&self, expr: &RascalExpression, values: &HashMap<String, RascalValue>, world: &mut World) -> Option<RascalValue> {

        match expr {
            RascalExpression::Relation(a, op, b) => {
                let mut ia = self.interpret_expression(a.as_ref(), values, world).unwrap();
                let mut ib = self.interpret_expression(b.as_ref(), values, world).unwrap();

                if let RascalValue::Entity(n) = ia {
                    ia = RascalValue::Num(n as i32);
                }

                if let RascalValue::Entity(n) = ib {
                    ib = RascalValue::Num(n as i32);
                }

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

            RascalExpression::BoolOp(a, op, b) => {
                let ia = self.interpret_expression(a.as_ref(), values, world).unwrap();
                let ib = self.interpret_expression(b.as_ref(), values, world).unwrap();

                use RascalValue::*;
                match (ia, op, ib) {
                    (Bool(l), BoolOper::And, Bool(r)) => Some(Bool(l && r)),
                    (Bool(l), BoolOper::Or, Bool(r)) => Some(Bool(l || r)),
                    _ => None
                }
            }

            RascalExpression::Binary(a, op, b) => {
                let ia = self.interpret_expression(a.as_ref(), values, world).unwrap();
                let ib = self.interpret_expression(b.as_ref(), values, world).unwrap();

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
                            RascalValue::Field(f) => format!("<field {}>", f),
                        };
                        let new_index = get_or_insert_into_string_pool(&format!("{}{}", string, right));
                        Some(Text(new_index))
                    }
                    (lhs, Op::Add, Text(r)) => {
                        let string = retrieve_string(r);
                        let left = match lhs {
                            RascalValue::Num(v) => v.to_string(),
                            RascalValue::Bool(b) => b.to_string(),
                            RascalValue::Text(t) => retrieve_string(t),
                            RascalValue::Symbol(c) => c.to_string(),
                            RascalValue::Entity(u) => format!("#{}", u),
                            RascalValue::Field(f) => format!("<field {}>", f),
                        };
                        let new_index = get_or_insert_into_string_pool(&format!("{}{}", left, string));
                        Some(Text(new_index))
                    }
                    _ => None
                }
            }

            RascalExpression::Unary(Un::Not, a) => {
                let ia = self.interpret_expression(a.as_ref(), values, world).unwrap();

                use RascalValue::*;
                match ia {
                    Bool(b) => Some(Bool(!b)),
                    _ => None
                }
            }

            RascalExpression::Unary(Un::Neg, a) => {
                let ia = self.interpret_expression(a.as_ref(), values, world).unwrap();

                use RascalValue::*;
                match ia {
                    Num(b) => Some(Num(-b)),
                    _ => None
                }
            }

            RascalExpression::Bool(b) => self.interpret_expression(b.as_ref(), values, world),
            RascalExpression::Num(n) => self.interpret_expression(n.as_ref(), values, world),
            RascalExpression::Symbol(s) => self.interpret_expression(s.as_ref(), values, world),

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
            RascalExpression::Tag(c) => {
                let owner = if c.is_owned { c.owner.as_ref().unwrap().as_str() } else { "_" };
                if let RascalValue::Entity(index) = values.get(owner).unwrap() {
                    assert_eq!(c.args.len(), 0);
                    let storage = world.storage_bitmaps.get(&*c.name).unwrap().clone();
                    Some(RascalValue::Bool(storage.get(*index)))
                } else {
                    unreachable!()
                }
            }
            RascalExpression::Identifier(ident) => {
                let index = get_or_insert_into_string_pool(ident);
                if world.unique_storage.contains_key(&index) {
                    Some(world.unique_storage.get(&index).unwrap().clone())
                } else {
                    None
                }
            }
            other => {
                println!("[parse_expression] failed to catch: {:?}", other);
                None
            }
        }
    }

    fn interpret_position_expression(&self, point: Option<RascalGlyphPosition>, values: &HashMap<String, RascalValue>, world: &mut World) -> Option<(u32, u32)> {
        if let Some(position) = point {
            let x = self.interpret_expression(&position.0, values, world).unwrap();
            let y = self.interpret_expression(&position.1, values, world).unwrap();
            if let RascalValue::Num(x) = x {
                if let RascalValue::Num(y) = y {
                    return Some((x as u32, y as u32));
                }
            }
            None
        } else {
            None
        }
    }

    fn interpret_color_expression(&self, color: Option<RascalGlyphColor>, values: &HashMap<String, RascalValue>, world: &mut World) -> Option<(u8, u8, u8)> {
        if let Some(clr) = color {
            let h = self.interpret_expression(&clr.0, values, world).unwrap();
            let s = self.interpret_expression(&clr.1, values, world).unwrap();
            let v = self.interpret_expression(&clr.2, values, world).unwrap();

            Some((h.as_num().unwrap_or(0) as u8, s.as_num().unwrap_or(0) as u8, v.as_num().unwrap_or(255) as u8))
        } else { None }
    }

    fn interpret_statement(&mut self, statement: &RascalStatement, values: &mut HashMap<String, RascalValue>, world: &mut World) -> RascalInterpretResult {
        match statement {
            RascalStatement::Let(name, e) => {
                if let RascalExpression::Identifier(name) = name {
                    if values.contains_key(name) {
                        return RascalInterpretResult::Err(format!("Variable {} already defined", name));
                    }

                    let value = self.interpret_expression(e, values, world).unwrap();
                    self.commit_change(world, values, SemanticChange::LetBinding(name.clone(), value));
                } else {
                    return RascalInterpretResult::Err(format!("Expected identifier, got {:?}", name))
                }
            }

            RascalStatement::Assign(a, b) => {
                match a {
                    RascalExpression::Identifier(name) => {
                        if !values.contains_key(name) {
                            let name_index = get_or_insert_into_string_pool(&name);
                            if !world.unique_storage.contains_key(&name_index) {
                                return RascalInterpretResult::Err(format!("Variable '{}' not found", name));
                            }
                        }

                        let value = self.interpret_expression(b, values, world).unwrap();
                        self.commit_change(world, values, SemanticChange::ValueAssign(name.clone(), value));
                    }

                    RascalExpression::FieldAccessor(name, x, y) => {
                        let width = world.size.0;
                        let name_index = get_or_insert_into_string_pool(&name);
                        if !world.field_storage.contains_key(&name_index) {
                            return RascalInterpretResult::Err(format!("Field '{}' not found", name));
                        } else {
                            let value = self.interpret_expression(b, values, world).unwrap().clone();
                            if world.field_storage.get(&name_index).unwrap().datatype != value.get_datatype() {
                                return RascalInterpretResult::Err(format!("Cannot assign value of different type to field {}", name));
                            }
                            if let Some(RascalValue::Num(x)) = self.interpret_expression(x, values, world) {
                                if let Some(RascalValue::Num(y)) = self.interpret_expression(y, values, world) {
                                    world.field_storage.get_mut(&name_index).unwrap().map.insert((y * width as i32 + x) as usize, value);
                                }
                            }
                        }
                    }


                    RascalExpression::ArrayAccessor(name, index) => {
                        let width = world.size.0;
                        let name_index = get_or_insert_into_string_pool(&name);
                        if !world.field_storage.contains_key(&name_index) {
                            return RascalInterpretResult::Err(format!("Field '{}' not found", name));
                        } else {
                            let value = self.interpret_expression(b, values, world).unwrap().clone();
                            if world.field_storage.get(&name_index).unwrap().datatype != value.get_datatype() {
                                return RascalInterpretResult::Err(format!("Cannot assign value of different type to field {}", name));
                            }
                            if let Some(RascalValue::Num(index)) = self.interpret_expression(index, values, world) {
                                world.field_storage.get_mut(&name_index).unwrap().map.insert(index as usize, value);
                            }
                        }
                    }

                    _ => {
                        return RascalInterpretResult::Err(format!("Expected identifier, got {:?}", a));
                    }
                }
            }

            RascalStatement::IfElse(body, then, or_else) => {
                match self.interpret_expression(body, values, world) {
                    Some(RascalValue::Bool(is)) => {
                        if is {
                            self.interpret_block(then.as_ref(), values, world);
                        } else if or_else.is_some() {
                            self.interpret_block(or_else.as_ref().unwrap(), values, world);
                        }
                    }

                    Some(other) => {
                        return RascalInterpretResult::Err(format!("If guard didn't evaluate to boolean, but rather to: {:?}!", other));
                    }

                    None => {
                        return RascalInterpretResult::Err(format!("It was impossible to interpret the evaluated expression: {:?}\n\t{:?}", body, values));
                    }
                }
            }

            RascalStatement::Spawn(a, b) => {
                if let RascalExpression::Identifier(name) = a {
                    self.commit_change(world, values, SemanticChange::SpawnEntity(name.clone(), b.clone()));
                } else {
                    return RascalInterpretResult::Err(format!("Expected identifier, got {:?}", a));
                }
            }

            RascalStatement::Destroy(id) => {
                match id {
                    RascalExpression::Identifier(name) => {
                        self.commit_change(world, values, SemanticChange::DestroyEntity(name.clone()));
                    }

                    _ => {}
                }
            }

            RascalStatement::Update(x, y) => {
                if let RascalExpression::Identifier(name) = x {
                    self.commit_change(world, values, SemanticChange::UpdateComponents(name.clone(), y.clone()));
                } else {
                    return RascalInterpretResult::Err(format!("Expected identifier, got {:?}", x));
                }
            }

            RascalStatement::Trigger(e) => {
                self.commit_change(world, values, SemanticChange::TriggerEvent(e.clone()));
            }

            RascalStatement::ConsumeEvent => {
                let system = self.current_system.clone().unwrap();
                if system.activation_event.is_none() {
                    return RascalInterpretResult::Err(format!("Trying to consume event from a stateful system. Ignoring."));
                } else {
                    self.commit_change(world, values, SemanticChange::ConsumeEvent);
                }
            }

            RascalStatement::Paint(position, foreground, background) => {
                let paint_all = position.is_none();

                let fg = self.interpret_color_expression(foreground.clone(), values, world);
                let bg = self.interpret_color_expression(background.clone(), values, world);

                if paint_all {
                    self.commit_change(world, values, SemanticChange::Paint(Position::All, fg.map(|f| Color::from(f)), bg.map(|b| Color::from(b))));
                } else {
                    let position = self.interpret_position_expression(position.clone(), values, world).unwrap();
                    self.commit_change(world, values, SemanticChange::Paint(Position::At(position), fg.map(|f| Color::from(f)), bg.map(|b| Color::from(b))));
                }
            }

            RascalStatement::Print(position, foreground, background,e) => {
                let xy = self.interpret_position_expression(Some(position.clone()), values, world).unwrap();
                let fg = self.interpret_color_expression(Some(foreground.clone()), values, world).unwrap_or((0, 0, 255));
                let bg = self.interpret_color_expression(Some(background.clone()), values, world).unwrap_or((0, 0, 0));

                self.commit_change(world, values, SemanticChange::Print(xy, fg, bg, e.clone()));
            }

            RascalStatement::Match(key, cases, then_block, else_block) => {
                let mut found_case = false;
                match self.interpret_expression(key, values, world) {
                    Some(value) => {
                        for (case_match, case_body) in cases {
                            let other = self.interpret_expression(case_match, values, world).unwrap();
                            if value == other {
                                found_case = true;
                                self.interpret_block(case_body, values, world);
                                break;
                            }
                        }
                    }

                    None => {
                        return RascalInterpretResult::Err(format!("It was impossible to interpret the key of the match: {:?}\n\t{:?}", key, values));
                    }
                }

                let block_to_call = if found_case { then_block } else { else_block };
                block_to_call.as_ref().map(|block| self.interpret_block(block, values, world));
            }

            RascalStatement::Inc(var, n) => {
                let mut result = RascalInterpretResult::Ok;

                if let RascalExpression::Identifier(name) = var {
                    if values.contains_key(name) {
                        let value = self.interpret_expression(n, values, world).unwrap();
                        self.commit_change(world, values, SemanticChange::ValueInc(name.clone(), value));
                    } else {
                        let name_index = get_or_insert_into_string_pool(&name);
                        let value = self.interpret_expression(n, values, world).unwrap();
                        if world.unique_storage.contains_key(&name_index) {
                            self.commit_change(world, values, SemanticChange::ValueInc(name.clone(), value));
                        } else {
                            result = RascalInterpretResult::Err(format!("Variable '{}' not found", name));
                        }
                    }
                } else {
                    return RascalInterpretResult::Err(format!("Expected identifier, got {:?}", var));
                }
            }

            RascalStatement::Dec(var, n) => {
                let mut result = RascalInterpretResult::Ok;

                if let RascalExpression::Identifier(name) = var {
                    if values.contains_key(name) {
                        let value = self.interpret_expression(n, values, world).unwrap();
                        self.commit_change(world, values, SemanticChange::ValueDec(name.clone(), value));
                    } else {
                        let name_index = get_or_insert_into_string_pool(&name);
                        let value = self.interpret_expression(n, values, world).unwrap();
                        if world.unique_storage.contains_key(&name_index) {
                            self.commit_change(world, values, SemanticChange::ValueDec(name.clone(), value));
                        } else {
                            result = RascalInterpretResult::Err(format!("Variable '{}' not found", name));
                        }
                    }
                } else {
                    return RascalInterpretResult::Err(format!("Expected identifier, got {:?}", var));
                }
            }

            RascalStatement::DebugPrint(expr) => {
                self.commit_change(world, values, SemanticChange::DebugPrint(expr.clone()));
            }

            RascalStatement::Quit => {
                self.commit_change(world, values, SemanticChange::Quit);
            }
        };

        RascalInterpretResult::Ok
    }

    fn interpret_block(&mut self, block: &RascalBlock, values: &mut HashMap<String, RascalValue>, world: &mut World) {
        for statement in block {
            if let RascalInterpretResult::Err(what) = self.interpret_statement(statement, values, world) {
                panic!("{}", what);
            }
        }
    }

    pub(crate) fn add_component(&self, world: &mut World, entity: EntityId, comp: ComponentCallSite, values: &HashMap<String, RascalValue>) {
        if let Some(comp_type) = world.component_types.get(&comp.name) {
            match comp_type {
                ComponentType::State => {
                    let mut args = Vec::new();

                    for arg in comp.args {
                        args.extend(self.interpret_expression(&arg.unwrap(), values, world).unwrap().emit());
                    }

                    world.add_component(entity, comp.name.as_str(), args);
                }
                ComponentType::Tag => {
                    if comp.modifier != ComponentModifier::Not {
                        world.add_tag(entity, comp.name.as_str());
                    }
                }

                _ => {
                    println!("Events and systems cannot be added to spawned entities: {}", comp.name);
                }
            }
        } else {
            println!("[!] {} not found.", comp.name);
        }
    }

    pub(crate) fn trigger_event(&self, world: &mut World, comp: ComponentCallSite, values: &HashMap<String, RascalValue>) {
        let args: Vec<_> = comp.args.iter().map(|arg| {
            let value_expr = arg.as_ref().unwrap();
            self.interpret_expression(value_expr, values, world).unwrap()
        }).collect();

        world.trigger_event(&comp.name, args);
    }

    pub(crate) fn assign_value(&mut self, world: &mut World, index: usize, name: String, new_value: RascalValue) {
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

    pub fn interpret_stateful(&mut self, world: &mut World, system: &SystemSignature) {
        self.bindings.clear();
        self.equalities.clear();
        self.current_event = None;
        self.current_system = Some(system.clone());
        self.interpret_with(world, system);
        self.current_system = None;
    }

    pub fn interpret_eventful(&mut self, world: &mut World, system: &SystemSignature,
                              event_descriptor: &ComponentSignature, chunk: BinaryComponent) -> Option<RascalEventfulResult> {

        BINDING_COUNTER.store(1, Ordering::SeqCst);

        self.current_system = Some(system.clone());

        self.bindings.clear();
        self.equalities.clear();

        let members_len = event_descriptor.members.len();

        let event_call_site = system.activation_event.as_ref().unwrap();
        if event_call_site.args.len() != members_len {
            println!("[System {}] Event {} has {} parameters, but only {} arguments found",
                     system.name, event_call_site.name, event_descriptor.members.len(), event_call_site.args.len());
            return None;
        }

        self.current_event = Some((event_descriptor.clone(), chunk));

        let members_in_order: Vec<_> = event_descriptor.ptrs.iter().map(|(_, mid)| mid.clone()).collect();
        let zipped_arg_and_ids: Vec<_> = event_call_site.args.iter().zip(members_in_order).collect();
        for (arg, id) in zipped_arg_and_ids {
            self.update_equality(&system.name, world, arg, &id, &event_descriptor);
        }

        self.interpret_with(world, system);

        self.current_event = None;
        self.current_system = None;

        let result = if self.event_consumed {
            Some(RascalEventfulResult::Consumed)
        } else {
            Some(RascalEventfulResult::Propagated)
        };

        self.event_consumed = false;
        result
    }
}