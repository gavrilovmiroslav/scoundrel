use pest;
use pest::iterators::Pairs;
use pest::Parser;
use pest_derive;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "ecs/grammar.pest"]
struct RascalParser;

pub fn parse_rascal(src: &str) {
    let pairs = RascalParser::parse(Rule::program, src).unwrap_or_else(|e| panic!("{}", e));

    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        println!("Text:    {}", pair.as_str());
    }
}