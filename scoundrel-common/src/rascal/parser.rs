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

use lazy_static;
use crate::rascal::interpreter::{get_or_insert_into_string_pool, RascalValue};
use crate::rascal::interpreter::RascalValue::{Entity, Symbol};

use crate::rascal::parser::RascalExpression::{BoolLiteral, FieldAccessor, Identifier, NumLiteral, RandomNumLiteral, TextLiteral};
use crate::rascal::parser::TokenType::{NonTerm, Term};

#[derive(Parser)]
#[grammar = "rascal/grammar.pest"]
struct RascalParser;

lazy_static! {
    static ref EXPR_PREC_CLIMBER: PrecClimber<Rule> = {
        PrecClimber::new(vec![
            Operator::new(Rule::and_op, Left) |
            Operator::new(Rule::or_op, Left),

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
        ])
    };
}


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum DataType {
    Num,
    Bool,
    Text,
    Entity,
    Symbol,
    Field,
    Set,
    Range,
}

impl DataType {
    pub fn parse_datatype(text: &str) -> DataType {
        use DataType::*;
        match text {
            "num" => Num,
            "text" => Text,
            "bool" => Bool,
            "entity" => Entity,
            "symbol" => Symbol,
            "set" => Set,
            _ if text.starts_with("field") => Field,
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
            Symbol => 2, // u16
            Field => 4, // u32
            Set => 0,
            Range => 8, // 2 * i32
        }
    }

    fn default(&self) -> RascalValue {
        use DataType::*;
        match self {
            Num => RascalValue::Num(0),
            Bool => RascalValue::Bool(false),
            Text => RascalValue::Text(0),
            Entity => RascalValue::Entity(0),
            Symbol => RascalValue::Symbol(0),
            Set => panic!("Cannot default a set!"),
            Field => panic!("Cannot default a field!"),
            Range => RascalValue::Range(0, 0),
        }
    }
}

pub(crate) type MemberId = String;

#[derive(Debug, Clone)]
pub struct MemberSignature {
    pub name: String,
    pub typ: DataType,
    pub offset: u8,
}

#[derive(Clone)]
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
pub enum ComponentModifier {
    Default,
    Not,
    Toggle,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum ComponentOrUnique {
    Comp(ComponentCallSite),
    Uniq(String),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct ComponentCallSite {
    pub modifier: ComponentModifier,
    pub is_owned: bool,
    pub owner: Option<String>,
    pub name: String,
    pub args: Vec<Option<RascalExpression>>,
}

impl ComponentCallSite {
    pub fn negated(self) -> Self {
        let is_owned = self.is_owned;
        let owner = self.owner;
        let name = self.name;
        let args = self.args;
        ComponentCallSite{ modifier: ComponentModifier::Not, is_owned, owner, name, args }
    }

    pub fn toggled(self) -> Self {
        let is_owned = self.is_owned;
        let owner = self.owner;
        let name = self.name;
        let args = self.args;
        ComponentCallSite{ modifier: ComponentModifier::Toggle, is_owned, owner, name, args }
    }
}

pub type SystemPrioritySize = u16;

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum SystemPriority {
    Default,
    Last,
    First,
    Game,
    UI,
    At(SystemPrioritySize),
}

#[derive(Debug, Clone)]
pub struct SystemSignature {
    pub id: u64,
    pub name: String,
    pub priority: SystemPriority,
    pub real_priority: SystemPrioritySize,
    pub activation_event: Option<ComponentCallSite>,
    pub storage_requirements: Vec<ComponentCallSite>,
    pub unique_requirements: Vec<String>,
    pub body_block: RascalBlock,
}

impl SystemSignature {
    pub fn with_id(self, id: u64) -> Self {
        SystemSignature{
            id, name: self.name, priority: SystemPriority::Default,
            activation_event: self.activation_event,
            real_priority: 0,
            storage_requirements: self.storage_requirements,
            unique_requirements: self.unique_requirements,
            body_block: self.body_block
        }
    }
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum BoolOper { And, Or }
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Rel { Eq, Ne, Le, Lt, Ge, Gt }
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Op { Add, Sub, Mul, Div, Mod }
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum Un { Not, Neg }

pub enum GeometryOp { Un, Int, Diff }

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum GeometryQuery {
    Rect(Box<RascalExpression>, Box<RascalExpression>,
         Box<RascalExpression>, Box<RascalExpression>),
    Circle(Box<RascalExpression>, Box<RascalExpression>, Box<RascalExpression>),
    Line(Box<RascalExpression>, Box<RascalExpression>,
         Box<RascalExpression>, Box<RascalExpression>),
    Union(Box<RascalExpression>, Box<RascalExpression>),
    Intersect(Box<RascalExpression>, Box<RascalExpression>),
    Diff(Box<RascalExpression>, Box<RascalExpression>),
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum RascalExpression {
    BoolOp(Box<RascalExpression>, BoolOper, Box<RascalExpression>),
    Relation(Box<RascalExpression>, Rel, Box<RascalExpression>),
    Binary(Box<RascalExpression>, Op, Box<RascalExpression>),
    Unary(Un, Box<RascalExpression>),
    Bool(Box<RascalExpression>),
    Num(Box<RascalExpression>),
    Symbol(Box<RascalExpression>),
    Query(GeometryQuery),
    BoolLiteral(bool),
    NumLiteral(i32),
    RandomNumLiteral(Box<RascalExpression>, Box<RascalExpression>),
    SymbolLiteral(u16),
    TextLiteral(String),
    ArrayAccessor(String, Box<RascalExpression>),
    FieldAccessor(String, Box<RascalExpression>, Box<RascalExpression>),
    Identifier(String),
    Tag(ComponentCallSite),
}

pub enum TokenType<'i> {
    NonTerm(Pairs<'i, Rule>),
    Term(Pair<'i, Rule>),
}

pub type RascalGlyphPosition = (RascalExpression, RascalExpression);
pub type RascalGlyphColor = (RascalExpression, RascalExpression, RascalExpression);

#[derive(Debug, Clone)]
pub enum RascalStatement {
    Assign(RascalExpression, RascalExpression),
    IfElse(RascalExpression, Box<RascalBlock>, Option<Box<RascalBlock>>),
    Spawn(RascalIdentifier, Vec<ComponentCallSite>),
    Destroy(RascalIdentifier),
    Update(RascalIdentifier, Vec<ComponentCallSite>),
    Trigger(ComponentCallSite),
    Paint(Option<RascalGlyphPosition>,
          Option<RascalGlyphColor>,
          Option<RascalGlyphColor>),
    Print(RascalGlyphPosition,
          RascalGlyphColor,
          RascalGlyphColor,
          RascalExpression),
    ConsumeEvent,
    Match(RascalIdentifier, Vec<(RascalExpression, RascalBlock)>, Option<RascalBlock>, Option<RascalBlock>),
    Inc(RascalExpression, RascalExpression),
    Dec(RascalExpression, RascalExpression),
    DebugPrint(RascalExpression),
    Let(RascalIdentifier, RascalExpression),
    ForLoopStatement(RascalIdentifier, (RascalExpression, RascalExpression), RascalBlock),
    Quit,
}

pub type RascalIdentifier = RascalExpression; /* could only be Identifier */
pub type RascalBlock = Vec<RascalStatement>;

impl SystemSignature {
    fn parse_simple_call_site(rule: Pair<Rule>) -> Option<ComponentCallSite> {
        let (name, args) = Self::parse_name_and_args(rule.into_inner());
        Some(ComponentCallSite {
            modifier: ComponentModifier::Default,
            is_owned: false,
            owner: None,
            name,
            args,
        })
    }

    fn parse_complex_call_site(rule: Pair<Rule>) -> Option<ComponentCallSite> {
        let mut owner = None;
        let mut call = rule.into_inner();
        if let Rule::owner_name = call.peek().unwrap().as_rule() {
            owner = Some(call.next().unwrap().as_str().to_string());
        }
        let (name, args) = Self::parse_name_and_args(call);

        Some(ComponentCallSite{ modifier: ComponentModifier::Default, is_owned: true, owner,
            name: name.to_string(), args })
    }

    fn parse_block_body(rule: Pair<Rule>) -> Vec<RascalStatement> {
        use TokenType::*;

        let mut result = Vec::new();
        for statement in rule.into_inner() {
            result.extend(Self::parse_statement(statement));
        }

        result
    }

    fn parse_array_accessor(exprs: &mut Pairs<Rule>) -> Option<RascalExpression> {
        Self::parse_general_accessor(exprs.next().unwrap())
    }

    fn parse_field_accessor(exprs: &mut Pairs<Rule>) -> Option<RascalExpression> {
        Self::parse_general_accessor(exprs.next().unwrap())
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
                EXPR_PREC_CLIMBER.climb(
                    expr,
                    |pair| match pair.as_rule() {
                        Rule::random => {
                            let mut rnd = pair.into_inner();
                            let a = Self::parse_expression(TokenType::Term(rnd.next().unwrap())).unwrap();
                            let b = Self::parse_expression(TokenType::Term(rnd.next().unwrap())).unwrap();
                            Some(RandomNumLiteral(Box::new(a), Box::new(b)))
                        },
                        Rule::bool_value => Some(BoolLiteral(pair.as_str().starts_with("true"))),
                        Rule::number => Some(NumLiteral(pair.as_str().trim().parse().unwrap())),
                        Rule::text_value => Some(TextLiteral({
                            let text = pair.as_str().to_string();
                            text[1..(text.len() - 1)].to_string()
                        })),
                        Rule::any_char => Some(RascalExpression::SymbolLiteral(pair.as_str().chars().next().unwrap() as u16)),
                        Rule::identifier | Rule::name => Some(RascalExpression::Identifier(pair.as_str().to_string())),
                        Rule::expression => Self::parse_expression(TokenType::NonTerm(pair.into_inner())),
                        Rule::complex_call_site => Some(RascalExpression::Tag(Self::parse_complex_call_site(pair).unwrap())),
                        Rule::negated_primary => {
                            let mut p = pair.into_inner();
                            let un = match p.next().unwrap().as_str() {
                                "!" => Un::Not,
                                "-" => Un::Neg,
                                _ => unreachable!()
                            };
                            let negated = p.next().unwrap();
                            if let Some(exp) = SystemSignature::parse_expression(
                                TokenType::NonTerm(negated.clone().into_inner())) {

                                Some(RascalExpression::Unary(un, Box::new(exp)))
                            } else {
                                if let Some(exp) = SystemSignature::parse_expression(TokenType::Term(negated)) {
                                    Some(RascalExpression::Unary(un, Box::new(exp)))
                                } else {
                                    None
                                }
                            }
                        }
                        Rule::field_accessor => {
                            let mut field = pair.into_inner();
                            let name = field.next().unwrap().as_str();
                            let x = Self::parse_expression(TokenType::NonTerm(field.next().unwrap().into_inner()));
                            let y = Self::parse_expression(TokenType::NonTerm(field.next().unwrap().into_inner()));

                            if let Some(x) = x {
                                if let Some(y) = y {
                                    return Some(RascalExpression::FieldAccessor(
                                        name.to_string(),
                                        Box::new(x),
                                        Box::new(y)))
                                }
                            }

                            None
                        }
                        Rule::array_accessor => {
                            let mut field = pair.into_inner();
                            let name = field.next().unwrap().as_str();
                            let index = Self::parse_expression(TokenType::NonTerm(field.next().unwrap().into_inner()));

                            if let Some(index) = index {
                                return Some(RascalExpression::ArrayAccessor(
                                    name.to_string(),
                                    Box::new(index)))
                            }

                            None
                        }

                        Rule::rect_query => {
                            let mut rect = pair.into_inner();
                            let x1 = Self::parse_expression(TokenType::NonTerm(rect.next().unwrap().into_inner())).unwrap();
                            let y1 = Self::parse_expression(TokenType::NonTerm(rect.next().unwrap().into_inner())).unwrap();
                            let x2 = Self::parse_expression(TokenType::NonTerm(rect.next().unwrap().into_inner())).unwrap();
                            let y2 = Self::parse_expression(TokenType::NonTerm(rect.next().unwrap().into_inner())).unwrap();
                            Some(RascalExpression::Query(GeometryQuery::Rect(Box::new(x1), Box::new(y1),
                                                                             Box::new(x2), Box::new(y2))))
                        }

                        Rule::circle_query => {
                            let mut circ = pair.into_inner();
                            let x = Self::parse_expression(TokenType::NonTerm(circ.next().unwrap().into_inner())).unwrap();
                            let y = Self::parse_expression(TokenType::NonTerm(circ.next().unwrap().into_inner())).unwrap();
                            let r = Self::parse_expression(TokenType::NonTerm(circ.next().unwrap().into_inner())).unwrap();
                            Some(RascalExpression::Query(GeometryQuery::Circle(Box::new(x), Box::new(y),
                                                                               Box::new(r))))
                        }

                        Rule::line_query => {
                            let mut line = pair.into_inner();
                            let x1 = Self::parse_expression(TokenType::NonTerm(line.next().unwrap().into_inner())).unwrap();
                            let y1 = Self::parse_expression(TokenType::NonTerm(line.next().unwrap().into_inner())).unwrap();
                            let x2 = Self::parse_expression(TokenType::NonTerm(line.next().unwrap().into_inner())).unwrap();
                            let y2 = Self::parse_expression(TokenType::NonTerm(line.next().unwrap().into_inner())).unwrap();
                            Some(RascalExpression::Query(GeometryQuery::Line(Box::new(x1), Box::new(y1),
                                                                             Box::new(x2), Box::new(y2))))
                        }

                        Rule::union_query => {
                            let mut un = pair.into_inner();
                            let d1 = un.next().unwrap();
                            let e1 = Self::parse_expression(TokenType::Term(d1)).unwrap();
                            let d2 = un.next().unwrap();
                            let e2 = Self::parse_expression(TokenType::Term(d2)).unwrap();

                            Some(RascalExpression::Query(GeometryQuery::Union(Box::new(e1), Box::new(e2))))
                        }

                        Rule::intersect_query => {
                            let mut int = pair.into_inner();
                            let d1 = int.next().unwrap();
                            let e1 = Self::parse_expression(TokenType::Term(d1)).unwrap();
                            let d2 = int.next().unwrap();
                            let e2 = Self::parse_expression(TokenType::Term(d2)).unwrap();

                            Some(RascalExpression::Query(GeometryQuery::Intersect(Box::new(e1), Box::new(e2))))
                        }

                        Rule::diff_query => {
                            let mut diff = pair.into_inner();
                            let d1 = diff.next().unwrap();
                            let e1 = Self::parse_expression(TokenType::Term(d1)).unwrap();
                            let d2 = diff.next().unwrap();
                            let e2 = Self::parse_expression(TokenType::Term(d2)).unwrap();

                            Some(RascalExpression::Query(GeometryQuery::Diff(Box::new(e1), Box::new(e2))))
                        }

                        e => {
                            println!("Returning none: {:?}", e);
                            None
                        }
                    },
                    |lhs, op, rhs| {
                        let lhs = lhs.unwrap();
                        let rhs = rhs.unwrap();
                        match op.as_rule() {

                            Rule::and_op => Some(RascalExpression::BoolOp(Box::new(lhs), BoolOper::And, Box::new(rhs))),
                            Rule::or_op => Some(RascalExpression::BoolOp(Box::new(lhs), BoolOper::Or, Box::new(rhs))),

                            Rule::eq_rel => Some(RascalExpression::Relation(Box::new(lhs), Rel::Eq, Box::new(rhs))),
                            Rule::ne_rel => Some(RascalExpression::Relation(Box::new(lhs), Rel::Ne, Box::new(rhs))),
                            Rule::lt_rel => Some(RascalExpression::Relation(Box::new(lhs), Rel::Lt, Box::new(rhs))),
                            Rule::gt_rel => Some(RascalExpression::Relation(Box::new(lhs), Rel::Gt, Box::new(rhs))),
                            Rule::le_rel => Some(RascalExpression::Relation(Box::new(lhs), Rel::Le, Box::new(rhs))),
                            Rule::ge_rel => Some(RascalExpression::Relation(Box::new(lhs), Rel::Ge, Box::new(rhs))),

                            Rule::plus_op => Some(RascalExpression::Binary(Box::new(lhs), Op::Add, Box::new(rhs))),
                            Rule::minus_op => Some(RascalExpression::Binary(Box::new(lhs), Op::Sub, Box::new(rhs))),
                            Rule::mul_op => Some(RascalExpression::Binary(Box::new(lhs), Op::Mul, Box::new(rhs))),
                            Rule::div_op => Some(RascalExpression::Binary(Box::new(lhs), Op::Div, Box::new(rhs))),
                            Rule::mod_op => Some(RascalExpression::Binary(Box::new(lhs), Op::Mod, Box::new(rhs))),

                            e => {
                                println!("Returning none: {:?}", e);
                                None
                            }
                        }
                    }
                )
            }

            TokenType::Term(expr) => {
                match expr.as_rule() {
                    Rule::random => {
                        let mut rnd = expr.into_inner();
                        let a = Self::parse_expression(TokenType::Term(rnd.next().unwrap())).unwrap();
                        let b = Self::parse_expression(TokenType::Term(rnd.next().unwrap())).unwrap();
                        Some(RandomNumLiteral(Box::new(a), Box::new(b)))
                    },
                    Rule::bool_value => Some(BoolLiteral(expr.as_str().starts_with("true"))),
                    Rule::number => Some(NumLiteral(expr.as_str().parse().unwrap())),
                    Rule::text_value => Some(TextLiteral(expr.as_str().to_string())),
                    Rule::identifier | Rule::name => Some(Identifier(expr.as_str().to_string())),
                    Rule::expression => Self::parse_expression(TokenType::NonTerm(expr.into_inner())),
                    Rule::array_accessor => Self::parse_array_accessor(&mut expr.into_inner()),
                    Rule::field_accessor => Self::parse_field_accessor(&mut expr.into_inner()),
                    Rule::rect_query => {
                        let mut rect = expr.into_inner();
                        let x1 = Self::parse_expression(TokenType::NonTerm(rect.next().unwrap().into_inner())).unwrap();
                        let y1 = Self::parse_expression(TokenType::NonTerm(rect.next().unwrap().into_inner())).unwrap();
                        let x2 = Self::parse_expression(TokenType::NonTerm(rect.next().unwrap().into_inner())).unwrap();
                        let y2 = Self::parse_expression(TokenType::NonTerm(rect.next().unwrap().into_inner())).unwrap();
                        Some(RascalExpression::Query(GeometryQuery::Rect(Box::new(x1), Box::new(y1),
                                                                         Box::new(x2), Box::new(y2))))
                    }

                    Rule::circle_query => {
                        let mut circ = expr.into_inner();
                        let x = Self::parse_expression(TokenType::NonTerm(circ.next().unwrap().into_inner())).unwrap();
                        let y = Self::parse_expression(TokenType::NonTerm(circ.next().unwrap().into_inner())).unwrap();
                        let r = Self::parse_expression(TokenType::NonTerm(circ.next().unwrap().into_inner())).unwrap();
                        Some(RascalExpression::Query(GeometryQuery::Circle(Box::new(x), Box::new(y),
                                                                           Box::new(r))))
                    }

                    Rule::line_query => {
                        let mut line = expr.into_inner();
                        let x1 = Self::parse_expression(TokenType::NonTerm(line.next().unwrap().into_inner())).unwrap();
                        let y1 = Self::parse_expression(TokenType::NonTerm(line.next().unwrap().into_inner())).unwrap();
                        let x2 = Self::parse_expression(TokenType::NonTerm(line.next().unwrap().into_inner())).unwrap();
                        let y2 = Self::parse_expression(TokenType::NonTerm(line.next().unwrap().into_inner())).unwrap();
                        Some(RascalExpression::Query(GeometryQuery::Line(Box::new(x1), Box::new(y1),
                                                                         Box::new(x2), Box::new(y2))))
                    }

                    _ => {
                        println!("WARNING: ADD THIS TERM TOO (line 314 in parser.rs): {:?}", expr.as_rule());
                        None
                    }
                }
            }
        }
    }

    fn parse_name_and_args(mut name_args: Pairs<Rule>) -> (String, Vec<Option<RascalExpression>>) {
        let name = name_args.next().unwrap().as_str();
        let mut collected_args = Vec::new();

        if let Some(_) = name_args.peek() {
            for arg in name_args.next().unwrap().into_inner() {
                collected_args.push(Self::parse_expression(TokenType::NonTerm(arg.into_inner())));
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
            Rule::toggled_call_site => {
                Self::parse_component_call_list(rule.into_inner().next().unwrap())
                    .map(|c| c.toggled())
            }
            _ => None,
        }
    }

    fn parse_with_clause(rule: Pair<Rule>) -> (Vec<ComponentCallSite>, Vec<String>) {
        let mut clauses = Vec::new();
        let mut uniques = Vec::new();
        let mut rule = rule.into_inner();
        let next = rule.next().unwrap();

        for next in next.into_inner() {
            if let Rule::unique_call = next.as_rule() {
                uniques.push(next.into_inner().next().unwrap().as_str().to_string());
            } else {
                clauses.push(Self::parse_component_call_list(next).unwrap());
            }
        }

        (clauses, uniques)
    }

    fn parse_position_expression(rule: &mut Pairs<Rule>) -> Option<(RascalExpression, RascalExpression)> {
        if let Rule::position = rule.peek().unwrap().as_rule() {
            let mut pos_rule = rule.next().unwrap().into_inner();
            let x = Self::parse_expression(NonTerm(pos_rule.next().unwrap().into_inner())).unwrap();
            let y = Self::parse_expression(NonTerm(pos_rule.next().unwrap().into_inner())).unwrap();
            Some((x, y))
        } else {
            None
        }
    }

    fn parse_color_expression(kind: Rule, rule: &mut Pairs<Rule>) -> Option<(RascalExpression, RascalExpression, RascalExpression)> {
        match rule.peek() {
            Some(r) if r.as_rule() == kind => {
                let mut fg_rule = rule.next().unwrap().into_inner();
                let h = Self::parse_expression(NonTerm(fg_rule.next().unwrap().into_inner())).unwrap();
                let s = Self::parse_expression(NonTerm(fg_rule.next().unwrap().into_inner())).unwrap();
                let v = Self::parse_expression(NonTerm(fg_rule.next().unwrap().into_inner())).unwrap();
                Some((h, s, v))
            }

            _ => {
                None
            }
        }
    }

    fn parse_if_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut if_statement = statement.into_inner();
        if let Some(guard) = Self::parse_expression(NonTerm(if_statement.next().unwrap().into_inner())) {
            let then_branch = Self::parse_block_body(if_statement.next().unwrap());
            result.push(RascalStatement::IfElse(guard, Box::new(then_branch),
                if_statement.next().map(|else_block| {
                    let mut else_piece = else_block.clone().into_inner().next().unwrap();
                    Box::new(Self::parse_else_statement(else_piece))
                })));
        }

        result
    }

    fn parse_else_statement(statement: Pair<Rule>) -> RascalBlock {
        match statement.as_rule() {
            Rule::block => Self::parse_block_body(statement),
            Rule::if_statement => Self::parse_statement(statement),
            _ => unreachable!(),
        }
    }

    fn parse_general_accessor(expr: Pair<Rule>) -> Option<RascalExpression> {
        match expr.as_rule() {
            Rule::field_accessor => {
                let mut child = expr.into_inner();
                let name = child.next().unwrap().as_str();
                if let Some(x) = Self::parse_expression(TokenType::NonTerm(child.next().unwrap().into_inner())) {
                    if let Some(y) = Self::parse_expression(TokenType::NonTerm(child.next().unwrap().into_inner())) {
                        return Some(RascalExpression::FieldAccessor(
                            name.to_string(),
                            Box::new(x),
                            Box::new(y)))
                    }
                }
            }

            Rule::array_accessor => {
                let mut child = expr.into_inner();
                let name = child.next().unwrap().as_str();
                if let Some(index) = Self::parse_expression(TokenType::NonTerm(child.next().unwrap().into_inner())) {
                    return Some(RascalExpression::ArrayAccessor(
                        name.to_string(),Box::new(index)))
                }
            }

            Rule::identifier => {
                return Self::parse_expression(TokenType::Term(expr))
            }

            _ => unreachable!()
        }

        None
    }

    fn parse_assignment(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut assign = statement.into_inner();
        let ass = assign.next().unwrap();
        let lhs = Self::parse_general_accessor(ass).unwrap();

        if let Some(rhs) = Self::parse_expression(NonTerm(assign.clone())) {
            result.push(RascalStatement::Assign(lhs, rhs));
        } else {
            unreachable!()
        }

        result
    }

    fn parse_self_mod_assignment(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut assign = statement.into_inner();
        let lhs = Self::parse_general_accessor(assign.next().unwrap()).unwrap();

        result.push(match assign.next().unwrap().as_str().trim() {
            "++" => RascalStatement::Inc(lhs, RascalExpression::NumLiteral(1)),
            "--" => RascalStatement::Dec(lhs, RascalExpression::NumLiteral(1)),
            _ => unreachable!()
        });

        result
    }

    fn parse_mod_assignment(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut assign = statement.into_inner();
        let mut lhs = Self::parse_general_accessor(assign.next().unwrap()).unwrap();

        result.push(match assign.next().unwrap().as_str().trim() {
            "+=" => RascalStatement::Inc(lhs, Self::parse_expression(NonTerm(assign.next().unwrap().into_inner())).unwrap()),
            "-=" => RascalStatement::Dec(lhs, Self::parse_expression(NonTerm(assign.next().unwrap().into_inner())).unwrap()),
            _ => unreachable!()
        });

        result
    }

    fn parse_for_loop_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut for_statement = statement.into_inner();

        if let Some(id @ RascalExpression::Identifier(_)) =
            Self::parse_expression(Term(for_statement.next().unwrap())) {

            if let Some(from) = Self::parse_expression(TokenType::Term(for_statement.next().unwrap())) {
                if let Some(to) = Self::parse_expression(TokenType::Term(for_statement.next().unwrap())) {
                    let body = Self::parse_block_body(for_statement.next().unwrap());

                    result.push(RascalStatement::ForLoopStatement(id, (from, to), body));
                }
            }
        }

        result
    }

    fn parse_spawn_or_update_statement(statement: Pair<Rule>) -> RascalBlock {
        let rule = statement.as_rule();
        let mut result = Vec::new();
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
        }

        result
    }

    fn parse_destroy_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut destroy_statement = statement.into_inner();
        if let Some(id @ RascalExpression::Identifier(_)) =
        Self::parse_expression(Term(destroy_statement.next().unwrap())) {
            result.push(RascalStatement::Destroy(id));
        }

        result
    }

    fn parse_trigger_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        if let Some(call_site) = Self::parse_call_site(statement.clone()) {
            result.push(RascalStatement::Trigger(call_site));
        }

        result
    }

    fn parse_paint_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut paint_statement = statement.into_inner();
        let position = Self::parse_position_expression(&mut paint_statement);
        let foreground = Self::parse_color_expression(Rule::foreground, &mut paint_statement);
        let background = Self::parse_color_expression(Rule::background, &mut paint_statement);

        if foreground.is_none() && background.is_none() {
            panic!("Both foreground and background are none.");
        }
        result.push(RascalStatement::Paint(position, foreground, background));

        result
    }

    fn parse_print_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut print_statement = statement.into_inner();
        let position = Self::parse_position_expression(&mut print_statement).unwrap_or(
            (RascalExpression::NumLiteral(0), RascalExpression::NumLiteral(0))
        );

        let foreground = Self::parse_color_expression(Rule::foreground, &mut print_statement).unwrap_or(
            (RascalExpression::NumLiteral(0), RascalExpression::NumLiteral(0), RascalExpression::NumLiteral(255))
        );

        let background = Self::parse_color_expression(Rule::background, &mut print_statement).unwrap_or(
            (RascalExpression::NumLiteral(0), RascalExpression::NumLiteral(0), RascalExpression::NumLiteral(0))
        );

        if let Some(expr) = Self::parse_expression(NonTerm(print_statement.next().unwrap().into_inner())) {
            result.push(RascalStatement::Print(position, foreground, background, expr));
        }

        result
    }

    fn parse_match_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut match_statement = statement.into_inner();
        let key = Self::parse_expression(TokenType::Term(match_statement.next().unwrap())).unwrap();
        let mut cases = Vec::new();
        let mut then_block = None;
        let mut else_block = None;

        for case in match_statement {
            match case.as_rule() {
                Rule::then_block => {
                    then_block = Some(Self::parse_block_body(case.into_inner().next().unwrap()).clone())
                }

                Rule::else_block => {
                    let mut inner_case = case.into_inner().next().unwrap();
                    else_block = Some(Self::parse_else_statement(inner_case));
                }

                _ => {
                    let mut case = case.into_inner();
                    let case_pattern = Self::parse_expression(TokenType::Term(case.next().unwrap())).unwrap();
                    let case_block = {
                        let statement = case.next().unwrap();

                        if let Rule::block = statement.as_rule() {
                            Self::parse_block_body(statement)
                        } else {
                            Self::parse_statement(statement)
                        }
                    };
                    cases.push((case_pattern, case_block));
                }
            }
        }

        result.push(RascalStatement::Match(key, cases, then_block, else_block));
        result
    }

    fn parse_debug_print_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        let mut print_statement = statement.into_inner();
        if let Some(expr) = Self::parse_expression(NonTerm(print_statement.next().unwrap().into_inner())) {
            result.push(RascalStatement::DebugPrint(expr));
        }

        result
    }

    fn parse_let_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();
        for binding in statement.into_inner() {
            let mut bound = binding.into_inner();

            if let Some(id@RascalExpression::Identifier(_)) =
                Self::parse_expression(Term(bound.next().unwrap())) {

                if let Some(value) = Self::parse_expression(NonTerm(bound)) {
                    result.push(RascalStatement::Let(id, value));
                }
            }
        }
        result
    }

    // THIS IS A TEMPLATE FOR EASY COPY-PASTING WHEN NEEDED ;)
    fn parse_template(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();

        result
    }

    fn parse_statement(statement: Pair<Rule>) -> RascalBlock {
        let mut result = Vec::new();

        match statement.as_rule() {
            Rule::assignment => result.extend(Self::parse_assignment(statement)),
            Rule::self_mod_assignment => result.extend(Self::parse_self_mod_assignment(statement)),
            Rule::mod_assignment => result.extend(Self::parse_mod_assignment(statement)),
            Rule::if_statement => result.extend(Self::parse_if_statement(statement)),
            Rule::spawn_statement |
            Rule::update_statement => result.extend(Self::parse_spawn_or_update_statement(statement)),
            Rule::destroy_statement => result.extend(Self::parse_destroy_statement(statement)),
            Rule::trigger_statement => result.extend(Self::parse_trigger_statement(statement)),
            Rule::paint_statement => result.extend(Self::parse_paint_statement(statement)),
            Rule::print_statement => result.extend(Self::parse_print_statement(statement)),
            Rule::match_statement => result.extend(Self::parse_match_statement(statement)),
            Rule::debug_print_statement => result.extend(Self::parse_debug_print_statement(statement)),
            Rule::let_statement => result.extend(Self::parse_let_statement(statement)),
            Rule::consume_statement => result.push(RascalStatement::ConsumeEvent),
            Rule::quit_statement => result.push(RascalStatement::Quit),
            Rule::for_loop_statement => result.extend(Self::parse_for_loop_statement(statement)),

            Rule::COMMENT => {}

            _ => {
                println!("Unknown in parse_body: {} {:?}!", statement.as_str(), statement.as_rule());
                unreachable!()
            }
        }

        result
    }

    pub fn parse_system(mut rule: Pairs<Rule>) -> Self {
        let mut system_title = rule.next().unwrap().into_inner();
        let name = system_title.next().unwrap().as_str().to_string();

        let priority = system_title.next().map(|p| match p.as_str() {
            "last" => SystemPriority::Last,
            "first" => SystemPriority::Default,
            "game" => SystemPriority::Game,
            "ui" => SystemPriority::UI,
            num => {
                if let Ok(v) = num.parse::<SystemPrioritySize>() {
                    SystemPriority::At(v)
                } else {
                    SystemPriority::Default
                }
            }
        }).unwrap_or(SystemPriority::Default);

        let mut event = None;
        let mut with = (Vec::new(), Vec::new());
        let mut body = Vec::new();

        for r in rule {
            match r.as_rule() {
                Rule::on_event_clause => event = Self::parse_call_site(r),
                Rule::with_query_clause => with = Self::parse_with_clause(r),
                Rule::block => body = Self::parse_block_body(r),
                _ => {}
            }
        }

        if with.0.len() == 0 {
            with.0.push(ComponentCallSite{
                modifier: ComponentModifier::Default,
                is_owned: true,
                owner: None,
                name: "Main".to_string(),
                args: vec![]
            });
        }

        SystemSignature{ id: 0, name, priority, activation_event: event, real_priority: 0, storage_requirements: with.0, unique_requirements: with.1, body_block: body }
    }
}

#[derive(Debug)]
pub enum RascalStruct {
    State(ComponentSignature),
    Event(ComponentSignature),
    Tag(String),
    System(SystemSignature),
    Unique(String, DataType, Option<DataType>, RascalValue),
}

impl RascalStruct {
    pub fn is_component(&self) -> bool {
        use RascalStruct::*;
        match self {
            System(_) => false,
            _ => true,
        }
    }

    pub fn get_type(&self) -> ComponentType {
        match self {
            RascalStruct::State(_) => ComponentType::State,
            RascalStruct::Event(_) => ComponentType::Event,
            RascalStruct::Tag(_) => ComponentType::Tag,
            RascalStruct::System(_) => ComponentType::System,
            RascalStruct::Unique(_, _, _, _) => ComponentType::Unique,
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            RascalStruct::State(state) => state.name.as_str(),
            RascalStruct::Event(event) => event.name.as_str(),
            RascalStruct::Tag(tag) => tag,
            RascalStruct::System(system) => system.name.as_str(),
            RascalStruct::Unique(name, _, _, _) => name,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComponentType {
    State, Event, Tag, System, Unique,
}

pub fn parse_rascal(src: &str) -> Vec<RascalStruct> {
    fn parse_component_name_and_members(mut rule: pest::iterators::Pairs<Rule>) -> (&str, Vec<(&str, DataType)>){
        let name = rule.next().unwrap().as_str();
        let members: Vec<(&str, DataType)> = if let Some(_) = rule.peek() {
            rule.next().unwrap().into_inner().map(|p| {
                let mut member_rule = p.into_inner();
                let member_name = member_rule.next().unwrap().as_str();
                let datatype_name = DataType::parse_datatype(member_rule.next().unwrap().as_str());
                (member_name, datatype_name)
            }).collect()
        } else {
            Vec::new()
        };

        (name, members)
    }

    let program = RascalParser::parse(Rule::program, src.trim()).unwrap_or_else(|e| panic!("{}", e));

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
                ast.push(RascalStruct::System(SystemSignature::parse_system(inner)));
            }

            Rule::unique_decl => {
                let mut inner = parse_tree.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let datatype = inner.next().unwrap();
                match datatype.as_rule() {
                    Rule::field => {
                        let name_index = get_or_insert_into_string_pool(&name);
                        let field = RascalValue::Field(name_index);
                        let datatype = DataType::parse_datatype(datatype.into_inner().next().unwrap().as_str());
                        let uniq = RascalStruct::Unique(name, DataType::Field, Some(datatype), field);
                        ast.push(uniq);
                    },

                    Rule::scalar => {
                        let datatype = DataType::parse_datatype(datatype.as_str());
                        let uniq = RascalStruct::Unique(name, datatype, None, datatype.default());
                        ast.push(uniq);
                    },

                    _ => {
                        println!("{:?}", datatype);
                        unreachable!()
                    },
                }
            }

            Rule::COMMENT => {}

            e => {
                println!("Error: {:?}", e);
                unreachable!()
            }
        }
    }

    ast
}