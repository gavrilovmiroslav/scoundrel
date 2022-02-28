use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use lazy_static;
use lazy_static::*;

use pest;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "ecs/grammar.pest"]
struct RascalParser;

use pest::*;
use pest::prec_climber::*;
use pest::prec_climber::Assoc::*;
use crate::ecs::parser::RascalExpression::{BoolLiteral, NumLiteral, TextLiteral};

lazy_static! {
    static ref EXPR_PREC_CLIMBER: PrecClimber<Rule> = {
        PrecClimber::new(vec![
            Operator::new(Rule::eq_rel, Left) |
            Operator::new(Rule::ne_rel, Left) |
            Operator::new(Rule::lt_rel, Left) |
            Operator::new(Rule::gt_rel, Left) |
            Operator::new(Rule::le_rel, Left) |
            Operator::new(Rule::ge_rel, Left),

            Operator::new(Rule::plus_op, Left) |
            Operator::new(Rule::minus_op, Left),

            Operator::new(Rule::mul_op, Left) |
            Operator::new(Rule::div_op, Left) |
            Operator::new(Rule::mod_op, Left),

            Operator::new(Rule::not_op, Left) |
            Operator::new(Rule::neg_op, Left)
        ])
    };
}


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
pub struct ComponentArgument {
    pub name: String,
    pub value: Option<RascalExpression>,
}

impl ComponentArgument {
    pub fn simple(name: String) -> Self {
        Self { name, value: None }
    }

    pub fn new(name: String, value: Option<RascalExpression>) -> Self {
        Self { name, value }
    }
}

#[derive(Debug)]
pub struct ComponentCallSite {
    pub not: bool,
    pub is_owned: bool,
    pub owner: Option<String>,
    pub name: String,
    pub args: Vec<ComponentArgument>,
}

impl ComponentCallSite {
    pub fn simple(name: String, args: Vec<String>) -> Self {
        ComponentCallSite{ not: false, is_owned: false, owner: None, name,
            args: args.iter().map(|a| ComponentArgument::simple(a.clone())).collect() }
    }

    pub fn negated(self) -> Self {
        let is_owned = self.is_owned;
        let owner = self.owner;
        let name = self.name;
        let args = self.args;
        ComponentCallSite{ not: !self.not, is_owned, owner, name, args }
    }
}

#[derive(Debug)]
pub struct CodeBlock {}

#[derive(Debug)]
pub struct SystemSignature {
    pub name: String,
    pub event: Option<ComponentCallSite>,
    pub with: Vec<ComponentCallSite>,
    pub body: Option<CodeBlock>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Rel { Eq, Ne, Le, Lt, Ge, Gt }
#[derive(Debug, PartialEq, Clone)]
pub enum Op { Add, Sub, Mul, Div, Mod }
#[derive(Debug, PartialEq, Clone)]
pub enum Un { Not, Neg }

#[derive(Debug, PartialEq, Clone)]
pub enum RascalExpression {
    Relation(Box<RascalExpression>, Rel, Box<RascalExpression>),
    Binary(Box<RascalExpression>, Op, Box<RascalExpression>),
    Unary(Un, Box<RascalExpression>),
    Bool(Box<RascalExpression>),
    Num(Box<RascalExpression>),
    Symbol(Box<RascalExpression>),
    Point(Box<RascalExpression>, Box<RascalExpression>),
    Color(Box<RascalExpression>, Box<RascalExpression>, Box<RascalExpression>),
    BoolLiteral(bool),
    NumLiteral(i32),
    SymbolLiteral(u16),
    TextLiteral(String),
    Identifier(String),
}

pub enum TokenType<'i> {
    NonTerm(Pairs<'i, Rule>),
    Term(Pair<'i, Rule>),
}

impl SystemSignature {
    fn parse_simple_call_site(rule: Pair<Rule>) -> Option<ComponentCallSite> {
        let (name, args) = Self::parse_name_and_args(rule.into_inner());
        Some(ComponentCallSite {
            not: false,
            is_owned: false,
            owner: None,
            name,
            args: args.iter().map(|(name, args)| ComponentArgument::new(name.clone(), args.clone())).collect()
        })
    }

    fn parse_complex_call_site(rule: Pair<Rule>) -> Option<ComponentCallSite> {
        let mut owner = None;
        let mut call = rule.into_inner();
        if let Rule::owner_name = call.peek().unwrap().as_rule() {
            owner = Some(call.next().unwrap().as_str().to_string());
        }
        let (name, args) = Self::parse_name_and_args(call);

        Some(ComponentCallSite{ not: false, is_owned: true, owner,
            name: name.to_string(),
            args: args.iter()
                .map(|(name, value)|
                    ComponentArgument::new(name.clone(), value.clone())).collect() })
    }

    fn parse_expression(exprs: TokenType) -> Option<RascalExpression> {
        fn recurse_by_token_type(mut pt: &mut Pairs<Rule>) -> Option<RascalExpression> {
            let xp = pt.next().unwrap();
            let cp = xp.clone();
            let xs = xp.into_inner();

            SystemSignature::parse_expression(if xs.peek().is_some() {
                TokenType::NonTerm(xs)
            } else {
                TokenType::Term(cp)
            })
        }

        match exprs {
            TokenType::NonTerm(expr) => {
                let result = EXPR_PREC_CLIMBER.climb(
                    expr,
                    |pair| match pair.as_rule() {
                        Rule::bool_value => BoolLiteral(pair.as_str().starts_with("true")),
                        Rule::number => NumLiteral(pair.as_str().trim().parse().unwrap()),
                        Rule::text_value => TextLiteral(pair.as_str().to_string()),
                        Rule::point_value => {
                            let mut pt = pair.into_inner();
                            let x = recurse_by_token_type(&mut pt);
                            let y = recurse_by_token_type(&mut pt);
                            println!("POINT: {:?} {:?}", x, y);
                            RascalExpression::Point(Box::new(x.unwrap()), Box::new(y.unwrap()))
                        }
                        Rule::color_value => {
                            let mut cl = pair.into_inner();
                            let r = recurse_by_token_type(&mut cl);
                            let g = recurse_by_token_type(&mut cl);
                            let b = recurse_by_token_type(&mut cl);
                            RascalExpression::Color(Box::new(r.unwrap()), Box::new(g.unwrap()), Box::new(b.unwrap()))
                        },
                        Rule::identifier => {
                            RascalExpression::Identifier(pair.as_str().to_string())
                        }
                        Rule::expression => {
                            Self::parse_expression(TokenType::NonTerm(pair.into_inner())).unwrap()
                        }
                        _ => {
                            println!("UNREACHABLE: {:?} {}", pair.as_rule(), pair.as_str());
                            unreachable!()
                        }
                    },
                    |lhs, op, rhs| match op.as_rule() {
                        Rule::eq_rel => RascalExpression::Relation(Box::new(lhs), Rel::Eq, Box::new(rhs)),
                        Rule::ne_rel => RascalExpression::Relation(Box::new(lhs), Rel::Ne, Box::new(rhs)),
                        Rule::lt_rel => RascalExpression::Relation(Box::new(lhs), Rel::Lt, Box::new(rhs)),
                        Rule::gt_rel => RascalExpression::Relation(Box::new(lhs), Rel::Gt, Box::new(rhs)),
                        Rule::le_rel => RascalExpression::Relation(Box::new(lhs), Rel::Le, Box::new(rhs)),
                        Rule::ge_rel => RascalExpression::Relation(Box::new(lhs), Rel::Ge, Box::new(rhs)),

                        Rule::plus_op => RascalExpression::Binary(Box::new(lhs), Op::Add, Box::new(rhs)),
                        Rule::minus_op => RascalExpression::Binary(Box::new(lhs), Op::Sub, Box::new(rhs)),
                        Rule::mul_op => RascalExpression::Binary(Box::new(lhs), Op::Mul, Box::new(rhs)),
                        Rule::div_op => RascalExpression::Binary(Box::new(lhs), Op::Div, Box::new(rhs)),
                        Rule::mod_op => RascalExpression::Binary(Box::new(lhs), Op::Mod, Box::new(rhs)),

                        Rule::not_op => RascalExpression::Unary(Un::Not, Box::new(lhs)),
                        Rule::neg_op => RascalExpression::Unary(Un::Neg, Box::new(lhs)),

                        _ => unreachable!()
                    }
                );

                Some(result)
            }

            TokenType::Term(expr) => {
                match expr.as_rule() {
                    Rule::bool_value => Some(BoolLiteral(expr.as_str().starts_with("true"))),
                    Rule::number => Some(NumLiteral(expr.as_str().parse().unwrap())),
                    Rule::text_value => Some(TextLiteral(expr.as_str().to_string())),
                    _ => {
                        println!("WARNING: WRITE THIS TERM TOO (line 314 in parser): {:?}", expr.as_rule());
                        None
                    }
                }
            }
        }
    }

    fn parse_name_and_args(mut name_args: Pairs<Rule>) -> (String, Vec<(String, Option<RascalExpression>)>) {
        let name = name_args.next().unwrap().as_str();
        let mut collected_args = Vec::new();

        if let Some(_) = name_args.peek() {
            for arg in name_args.next().unwrap().into_inner() {
                collected_args.push(match arg.as_rule() {
                    Rule::name_with_value => {
                        let mut name_value = arg.into_inner();
                        let name = name_value.next().unwrap().as_str();
                        let value = Self::parse_expression(TokenType::NonTerm(name_value));
                        (name.to_string(), value)
                    }
                    Rule::name => {
                        (arg.as_str().to_string(), None)
                    }
                    _ => {
                        println!("{:?}", arg.as_rule());
                        panic!("parse_name_and_args!");
                    }
                });
            }
        }

        (name.to_string(), collected_args)
    }

    fn parse_event_clause(rule: Pair<Rule>) -> Option<ComponentCallSite> {
        let mut rule = rule.into_inner();
        let next = rule.next().unwrap();

        match next.as_rule() {
            Rule::complex_call_site => Self::parse_complex_call_site(next),
            Rule::simple_call_site => Self::parse_simple_call_site(next),
            _ => unreachable!(),
        }
    }

    fn parse_component_call_list(rule: Pair<Rule>) -> Option<ComponentCallSite> {
        match rule.as_rule() {
            Rule::complex_call_site => Self::parse_complex_call_site(rule),
            Rule::simple_call_site => Self::parse_simple_call_site(rule),
            Rule::negated_call_site => {
                Self::parse_component_call_list(rule.into_inner().next().unwrap())
                    .map(|c| c.negated())
            }
            _ => None,
        }
    }

    fn parse_with_clause(rule: Pair<Rule>) -> Vec<ComponentCallSite> {
        let mut clauses = Vec::new();
        let mut rule = rule.into_inner();
        let next = rule.next().unwrap();

        for next in next.into_inner() {
            clauses.push(Self::parse_component_call_list(next).unwrap());
        }

        clauses
    }

    pub fn new(mut rule: Pairs<Rule>) -> SystemSignature {
        let name = rule.next().unwrap().as_str().to_string();
        let mut event = None;
        let mut with = Vec::new();
        let mut body = None;

        for r in rule {
            match r.as_rule() {
                Rule::on_event_clause => {
                    event = Self::parse_event_clause(r);
                }
                Rule::with_query_clause => {
                    with = Self::parse_with_clause(r);
                }
                Rule::system_block => {
                    println!("BLOCK: {}", r.as_str());
                }
                _ => {}
            }
        }
        SystemSignature{ name, event, with, body }
    }
}

#[derive(Debug)]
pub enum RascalStruct {
    State(ComponentSignature),
    Event(ComponentSignature),
    Tag(String),
    System(SystemSignature),
}

impl RascalStruct {
    pub fn is_component(&self) -> bool {
        use RascalStruct::*;
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

pub fn parse_rascal(src: &str) -> Vec<RascalStruct> {
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
                ast.push(RascalStruct::State(ComponentSignature::new(name, members)))
            }

            Rule::event_decl => {
                let mut event_rule = parse_tree.into_inner();
                let (name, members) = parse_component_name_and_members(event_rule);
                ast.push(RascalStruct::Event(ComponentSignature::new(name, members)))
            }

            Rule::tag_decl => {
                let mut inner = parse_tree.into_inner();
                let name = inner.next().unwrap().as_str();
                ast.push(RascalStruct::Tag(name.to_string()));
            }

            Rule::system_decl => {
                println!("\nSYSTEM!");
                let mut inner = parse_tree.into_inner();
                ast.push(RascalStruct::System(SystemSignature::new(inner)));
            }

            _ => {}
        }
    }

    ast
}