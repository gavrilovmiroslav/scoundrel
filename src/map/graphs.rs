use crate::distance;
use crate::map::{center, StencilImpl};
use petgraph::algo::min_spanning_tree;
use petgraph::data::FromElements;
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use std::collections::HashMap;
use std::vec::IntoIter;

pub struct MapGraph(
    pub Graph<StencilImpl, f32>,
    pub HashMap<StencilImpl, NodeIndex>,
);

impl MapGraph {
    pub fn new() -> MapGraph {
        MapGraph {
            0: Default::default(),
            1: Default::default(),
        }
    }

    pub fn add_node(&mut self, node: StencilImpl) {
        let ni = self.0.add_node(node.clone());
        self.1.insert(node, ni);
    }

    pub fn fill_graph(&mut self) {
        for (i1, node) in &self.1 {
            for (i2, other) in &self.1 {
                if node != other {
                    self.0
                        .add_edge(*node, *other, distance(center(i1), center(i2)));
                }
            }
        }
    }

    pub fn get_min_tree(&mut self) -> MapTree {
        self.fill_graph();
        MapTree {
            0: Graph::<StencilImpl, f32>::from_elements(min_spanning_tree(&self.0)),
        }
    }
}

pub struct MapTree(pub Graph<StencilImpl, f32>);

impl MapTree {}

impl IntoIterator for MapTree {
    type Item = (StencilImpl, StencilImpl, f32);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        let mut vec = Vec::new();

        let edges = self.0.edge_indices().into_iter();
        for edge in edges {
            let w = self.0.edge_weight(edge).unwrap();
            if let Some((a, b)) = self.0.edge_endpoints(edge) {
                vec.push((
                    self.0.node_weight(a).unwrap().clone(),
                    self.0.node_weight(b).unwrap().clone(),
                    *w,
                ));
            }
        }

        vec.into_iter()
    }
}
