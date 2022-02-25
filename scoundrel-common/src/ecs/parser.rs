use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use pest;
use pest::iterators::Pairs;
use pest::Parser;
use pest_derive;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "ecs/grammar.pest"]
struct RascalParser;

#[derive(Debug, Copy, Clone)]
pub enum DataType {
    Num,
    Text,
    Entity,
    Point,
    Symbol,
    Color,
}

impl DataType {
    pub fn parse_datatype(text: &str) -> DataType {
        use DataType::*;
        match text {
            "num" => Num,
            "text" => Text,
            "entity" => Entity,
            "point" => Point,
            "symbol" => Symbol,
            "color" => Color,
            _ => panic!("Wrong datatype: {}", text)
        }
    }

    pub fn size_in_bytes(&self) -> u8 {
        use DataType::*;
        match self {
            Num => 4, // u32
            Text => 4, // ptr size
            Entity => 8, // u64
            Point => 8, // 2x u32
            Symbol => 2, // u16
            Color => 4, // u32
        }
    }
}

type MemberId = String;

#[derive(Debug)]
pub struct MemberSignature {
    pub name: String,
    pub typ: DataType,
    pub ptr: u8,
}

pub struct ComponentSignature {
    pub name: String,
    pub size: u8,
    pub ptrs: Vec<(u8, MemberId)>,
    pub members: HashMap<MemberId, MemberSignature>,
}

impl ComponentSignature {
    fn get_ptrs(down: &Vec<(&str, DataType)>) -> (u8, Vec<(u8, MemberId)>) {
        let mut ptr_count = 0u8;
        let fields = down.iter().map(|(n, dt)| {
            let ptr = ptr_count;
            ptr_count += dt.size_in_bytes();
            (ptr, n.to_string())
        }).collect();
        (ptr_count, fields)
    }

    fn lift(down: &Vec<(&str, DataType)>) -> HashMap<MemberId, MemberSignature> {
        let mut ptr_count = 0u8;
        down.iter().map(|(n, dt)| {
            let ptr = ptr_count;
            ptr_count += dt.size_in_bytes();
            (n.to_string(), MemberSignature{ name: n.to_string(), typ: dt.clone(), ptr })
        }).collect()
    }

    pub fn new(name: &str, members: Vec<(&str, DataType)>) -> Self {
        let (size, ptrs) = ComponentSignature::get_ptrs(&members);
        Self { name: name.to_string(), size, ptrs, members: ComponentSignature::lift(&members) }
    }
}

impl Debug for ComponentSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let members: Vec<String> = self.ptrs.iter()
            .map(|(p, id)| format!("[{}-{}] {}: {:?}", p, self.members[id].typ.size_in_bytes() + p, id, self.members[id].typ))
            .collect();
        write!(f, "{} (size: {}) {{{}}}", self.name, self.size, members.join(", "))
    }
}

#[derive(Debug)]
pub struct SystemSignature {

}

impl SystemSignature {
    pub fn new(mut rule: pest::iterators::Pairs<Rule>) -> SystemSignature {
        let name = rule.next().unwrap().as_str();
        println!("NAME: {}", name);

        if let Rule::on_event_clause = rule.peek().unwrap().as_rule() {
            let on_clause = rule.next().unwrap().as_str();
            println!("ON: {}", on_clause);
        }

        if let Rule::with_query_clause = rule.peek().unwrap().as_rule() {
            let queries = rule.next().unwrap().as_str();
            println!("QUERIES: {}", queries);
        }

        SystemSignature{}
    }
}

#[derive(Debug)]
pub enum RascalValue {
    State(ComponentSignature),
    Event(ComponentSignature),
    Tag(String),
    System(SystemSignature),
}

impl RascalValue {
    pub fn is_component(&self) -> bool {
        use RascalValue::*;
        match self {
            System(_) => false,
            _ => true,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ComponentType {
    State, Event, Tag, System
}

pub fn parse_rascal(src: &str) -> Vec<RascalValue> {
    fn parse_component_name_and_members(mut rule: pest::iterators::Pairs<Rule>) -> (&str, Vec<(&str, DataType)>){
        let name = rule.next().unwrap().as_str();
        let members: Vec<(&str, DataType)> = rule.next().unwrap().into_inner().map(|p| {
            let mut member_rule = p.into_inner();
            let member_name = member_rule.next().unwrap().as_str();
            let datatype_name = DataType::parse_datatype(member_rule.next().unwrap().as_str());
            (member_name, datatype_name)
        }).collect();

        (name, members)
    }

    let program = RascalParser::parse(Rule::program, src).unwrap_or_else(|e| panic!("{}", e));

    let mut ast = Vec::new();

    for parse_tree in program {
        match parse_tree.as_rule() {
            Rule::state_decl => {
                println!("{}", parse_tree.as_str());
                let mut state_rule = parse_tree.into_inner();
                let (name, members) = parse_component_name_and_members(state_rule);
                ast.push(RascalValue::State(ComponentSignature::new(name, members)))
            }

            Rule::event_decl => {
                let mut event_rule = parse_tree.into_inner();
                let (name, members) = parse_component_name_and_members(event_rule);
                ast.push(RascalValue::Event(ComponentSignature::new(name, members)))
            }

            Rule::tag_decl => {
                let mut inner = parse_tree.into_inner();
                let name = inner.next().unwrap().as_str();
                ast.push(RascalValue::Tag(name.to_string()));
            }

            Rule::system_decl => {
                println!("\nSYSTEM!");
                let mut inner = parse_tree.into_inner();
                ast.push(RascalValue::System(SystemSignature::new(inner)));
            }

            _ => {}
        }
    }

    ast
}