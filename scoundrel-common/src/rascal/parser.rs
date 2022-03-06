use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use lazy_static::*;
use pest;
use pest::*;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest::prec_climber::*;
use pest::prec_climber::Assoc::*;
use pest_derive;
use pest_derive::Parser;
use show_my_errors::{AnnotationList, Stylesheet};

use lazy_static;

use crate::rascal::parser::RascalExpression::{BoolLiteral, Identifier, NumLiteral, TextLiteral};

#[derive(Parser)]
#[grammar = "rascal/grammar.pest"]
struct RascalParser;

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
    Bool,
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
            "bool" => Bool,
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
            Bool => 4, // bool
            Text => 4, // ptr size
            Entity => 8, // u64
            Point => 8, // 2x i32
            Symbol => 2, // u16
            Color => 3, // u32
        }
    }
}

pub(crate) type MemberId = String;

#[derive(Debug)]
pub struct MemberSignature {
    pub name: String,
    pub typ: DataType,
    pub offset: u8,
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
            (n.to_string(), MemberSignature{ name: n.to_string(), typ: dt.clone(), offset: ptr })
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

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
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

pub type SystemPrioritySize = u16;

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum SystemPriority {
    Default,
    Last,
    First,
    At(SystemPrioritySize),
}

#[derive(Debug, Clone)]
pub struct SystemSignature {
    pub id: u64,
    pub name: String,
    pub priority: SystemPriority,
    pub event: Option<ComponentCallSite>,
    pub with: Vec<ComponentCallSite>,
    pub body: RascalBlock,
}

impl SystemSignature {
    pub fn with_id(self, id: u64) -> Self {
        SystemSignature{
            id, name: self.name, priority: SystemPriority::Default,
            event: self.event, with: self.with, body: self.body }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Rel { Eq, Ne, Le, Lt, Ge, Gt }
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Op { Add, Sub, Mul, Div, Mod }
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Un { Not, Neg }

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
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

#[derive(Debug, Clone)]
pub enum RascalStatement {
    Assign(RascalExpression, RascalExpression),
    IfElse(RascalExpression, Box<RascalBlock>, Option<Box<RascalBlock>>),
    Spawn(RascalIdentifier, Vec<ComponentCallSite>),
    Destroy(RascalIdentifier),
    Update(RascalIdentifier, Vec<ComponentCallSite>),
    Trigger(ComponentCallSite),
    Print(RascalExpression),
    ConsumeEvent,
}

pub type RascalIdentifier = RascalExpression; /* could only be Identifier */
pub type RascalBlock = Vec<RascalStatement>;

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
                        Rule::text_value => TextLiteral({
                            let text = pair.as_str().to_string();
                            text[1..(text.len() - 1)].to_string()
                        }),
                        Rule::point_value => {
                            let mut pt = pair.into_inner();
                            let x = recurse_by_token_type(&mut pt);
                            let y = recurse_by_token_type(&mut pt);
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
                    Rule::identifier => Some(Identifier(expr.as_str().to_string())),
                    _ => {
                        println!("WARNING: ADD THIS TERM TOO (line 314 in parser.rs): {:?}", expr.as_rule());
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
                    Rule::anonymous_with_value => {
                        let value = Self::parse_expression(TokenType::NonTerm(arg.into_inner()));
                        ("_".to_string(), value)
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

    fn parse_call_site(rule: Pair<Rule>) -> Option<ComponentCallSite> {
        let mut rule = rule.into_inner();
        let next = rule.next().unwrap();

        match next.as_rule() {
            Rule::complex_call_site => Self::parse_complex_call_site(next),
            Rule::simple_call_site => Self::parse_simple_call_site(next),
            _ => {
                println!("UNREACHABLE: {} {:?}", next.as_str(), next.as_rule());
                unreachable!()
            },
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

    fn parse_body(rule: Pair<Rule>) -> Vec<RascalStatement> {
        use TokenType::*;

        let mut result = Vec::new();
        for statement in rule.into_inner() {
            match statement.as_rule() {

                Rule::assignment => {
                    let mut assign = statement.into_inner();
                    if let Some(id@RascalExpression::Identifier(_)) =
                            Self::parse_expression(Term(assign.next().unwrap())) {

                        if let Some(value) = Self::parse_expression(NonTerm(assign.clone())) {
                            result.push(RascalStatement::Assign(id, value));
                        } else {
                            let mut list = AnnotationList::new("script", assign.clone().as_str());
                            list.error(0..3, "Expression expected", "");
                            list.show_stdout(&Stylesheet::colored());
                        }
                    } else {
                        let mut list = AnnotationList::new("parser#assignment", assign.clone().as_str());
                        list.error(0..3, "Identifier expected", "(so, something like 'foo', 'x', etc.)");
                        list.show_stdout(&Stylesheet::colored());
                    }
                }

                Rule::if_statement => {
                    let mut if_statement = statement.into_inner();
                    if let Some(guard) = Self::parse_expression(NonTerm(if_statement.next().unwrap().into_inner())) {
                        let then_branch = Self::parse_body(if_statement.next().unwrap());
                        result.push(RascalStatement::IfElse(guard, Box::new(then_branch),
                            if_statement.next().map(|else_block| Box::new(Self::parse_body(else_block)))));
                    } else {
                        let mut list = AnnotationList::new("parser#if_statement", if_statement.clone().as_str());
                        list.error(0..3, "Expression expected", "");
                        list.show_stdout(&Stylesheet::colored());
                    }
                }

                rule@(Rule::spawn_statement | Rule::update_statement) => {
                    let mut spawn_statement = statement.into_inner();
                    if let Some(id @ RascalExpression::Identifier(_)) =
                            Self::parse_expression(Term(spawn_statement.next().unwrap())) {

                        let mut clauses = Vec::new();
                        for next in spawn_statement.next().unwrap().into_inner() {
                            clauses.push(Self::parse_component_call_list(next).unwrap());
                        }

                        result.push(match rule {
                            Rule::spawn_statement => RascalStatement::Spawn(id, clauses),
                            Rule::update_statement => RascalStatement::Update(id, clauses),
                            _ => unreachable!()
                        });
                    } else {
                        let mut list = AnnotationList::new("parser#spawn/update", spawn_statement.clone().as_str());
                        list.error(0..3, "Identifier expected", "(so, something like 'foo', 'x', etc.)");
                        list.show_stdout(&Stylesheet::colored());
                    }
                }

                Rule::destroy_statement => {
                    let mut destroy_statement = statement.into_inner();
                    if let Some(id @ RascalExpression::Identifier(_)) =
                            Self::parse_expression(Term(destroy_statement.next().unwrap())) {

                        result.push(RascalStatement::Destroy(id));
                    } else {
                        let mut list = AnnotationList::new("parser#destroy", destroy_statement.clone().as_str());
                        list.error(0..3, "Identifier expected", "(so, something like 'foo', 'x', etc.)");
                        list.show_stdout(&Stylesheet::colored());
                    }
                }

                Rule::trigger_statement => {
                    if let Some(call_site) = Self::parse_call_site(statement.clone()) {
                        result.push(RascalStatement::Trigger(call_site));
                    } else {
                        let mut list = AnnotationList::new("parser#trigger", statement.clone().as_str());
                        list.error(0..3, "Call site expected", "(so, something like 'Foo(3, 1)')");
                        list.show_stdout(&Stylesheet::colored());
                    }
                }

                Rule::consume_statement => {
                    result.push(RascalStatement::ConsumeEvent);
                }

                Rule::print_statement => {
                    let mut print_statement = statement.into_inner();
                    if let Some(expr) = Self::parse_expression(NonTerm(print_statement.next().unwrap().into_inner())) {
                        result.push(RascalStatement::Print(expr));
                    }
                }

                _ => {
                    println!("Unknown in parse_body: {}!", statement.as_str());
                    unreachable!()
                }
            }
        }

        result
    }

    pub fn new(mut rule: Pairs<Rule>) -> SystemSignature {
        let mut system_title = rule.next().unwrap().into_inner();
        let name = system_title.next().unwrap().as_str().to_string();

        let priority = system_title.next().map(|p| match p.as_str() {
            "last" => SystemPriority::Last,
            "first" => SystemPriority::Default,
            num => {
                if let Ok(v) = num.parse::<SystemPrioritySize>() {
                    SystemPriority::At(v)
                } else {
                    SystemPriority::Default
                }
            }
        }).unwrap_or(SystemPriority::Default);

        let mut event = None;
        let mut with = Vec::new();
        let mut body = Vec::new();

        for r in rule {
            match r.as_rule() {
                Rule::on_event_clause => event = Self::parse_call_site(r),
                Rule::with_query_clause => with = Self::parse_with_clause(r),
                Rule::system_block => body = Self::parse_body(r),
                _ => {}
            }
        }

        SystemSignature{ id: 0, name, priority, event, with, body }
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
                let mut inner = parse_tree.into_inner();
                ast.push(RascalStruct::System(SystemSignature::new(inner)));
            }

            _ => {}
        }
    }

    ast
}