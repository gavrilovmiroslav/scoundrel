use std::collections::{HashMap, HashSet};
use structopt::clap::Values;
use crate::rascal::interpreter::RascalValue;
use crate::rascal::parser::RascalExpression;

pub type ValueContext = ValueStack;

#[derive(Debug, Clone)]
pub enum Call {
    ByRef(String),
    ByVal(RascalValue),
}

impl Call {
    pub fn get_value(&self) -> Option<RascalValue> {
        match self {
            Call::ByRef(_) => None,
            Call::ByVal(e) => Some(e.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValueStack {
    stack: Vec<HashMap<String, Call>>,
    keys_cache: HashSet<String>,
}

impl Default for ValueStack {
    fn default() -> Self { ValueStack { stack: Vec::default(), keys_cache: HashSet::default() } }
}

impl ValueStack {
    pub fn push(&mut self) {
        self.stack.push(HashMap::new())
    }

    pub fn pop(&mut self) {
        if let Some(layer) = self.stack.pop() {
            for (k, v) in layer {
                if !self.contains_key(&k) {
                    self.keys_cache.remove(&k);
                }
            }
        }
    }

    pub fn add(&mut self, name: String, id: Call) {
        match id {
            Call::ByRef(i) => self.add_ident(name, i),
            Call::ByVal(e) => self.add_value(name, e),
        }
    }

    pub fn add_ident(&mut self, name: String, id: String) {
        self.stack.last_mut().unwrap().insert(name.clone(), Call::ByRef(id));
        self.keys_cache.insert(name);
    }

    pub fn add_value(&mut self, name: String, value: RascalValue) {
        self.stack.last_mut().unwrap().insert(name.clone(), Call::ByVal(value));
        self.keys_cache.insert(name);
    }

    pub fn add_all(&mut self, values: Vec<(String, Call)>) {
        for (arg, value) in values {
            self.add(arg, value);
        }
    }

    pub fn contains_key(&self, name: &String) -> bool {
        for layer in self.stack.iter().rev() {
            if layer.contains_key(name) {
                return true;
            }
        }

        false
    }

    pub fn keys(&self) -> std::vec::IntoIter<String> {
        self.keys_cache.iter().map(|x| x.clone()).collect::<Vec<String>>().into_iter()
    }

    pub fn get(&mut self, name: &String) -> Option<Call> {
        for layer in self.stack.iter().rev() {
            if layer.contains_key(name) {
                return Some(layer.get(name).unwrap().clone());
            }
        }

        None
    }

    pub fn get_mut(&mut self, name: String) -> Option<&mut Call> {
        for layer in self.stack.iter_mut().rev() {
            if layer.contains_key(&name) {
                return layer.get_mut(&name);
            }
        }

        None
    }
}

impl IntoIterator for ValueStack {
    type Item = (String, Call);
    type IntoIter = std::vec::IntoIter<(String, Call)>;

    fn into_iter(self) -> Self::IntoIter {
        let mut mapping = Vec::new();
        for layer in self.stack {
            for (name, value) in layer {
                mapping.push((name, value));
            }
        }

        mapping.into_iter()
    }
}

impl IntoIterator for &ValueStack {
    type Item = (String, Call);
    type IntoIter = std::vec::IntoIter<(String, Call)>;

    fn into_iter(self) -> Self::IntoIter {
        let mut mapping = Vec::new();
        for layer in &self.stack {
            for (name, value) in layer {
                mapping.push((name.clone(), value.clone()));
            }
        }

        mapping.into_iter()
    }
}