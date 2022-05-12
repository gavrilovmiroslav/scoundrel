use crate::{distance, Point};
use petgraph::algo::min_spanning_tree;
use petgraph::data::FromElements;
use petgraph::Graph;
use std::collections::HashSet;

pub fn min_span_tree(pts: &[Point]) -> Vec<(Point, Point)> {
    let mut g: Graph<Point, f32> = Graph::new();
    let nodes = pts
        .iter()
        .map(|pt| g.add_node(pt.clone()))
        .collect::<Vec<_>>();

    for node in &nodes {
        for other in &nodes {
            if node != other {
                let r1 = g.node_weight(*node).unwrap();
                let r2 = g.node_weight(*other).unwrap();
                let d = distance(*r1, *r2);
                g.add_edge(*node, *other, d);
            }
        }
    }

    let ming = Graph::<Point, f32>::from_elements(min_spanning_tree(&g));
    let mut vec = Vec::new();

    let edges = ming.edge_indices().into_iter();
    for edge in edges {
        let w = ming.edge_weight(edge).unwrap();
        if let Some((a, b)) = ming.edge_endpoints(edge) {
            vec.push((
                ming.node_weight(a).unwrap().clone(),
                ming.node_weight(b).unwrap().clone(),
            ));
        }
    }

    vec
}

pub fn planar_map(pts: &[Point]) -> Vec<(Point, Point)> {
    let del = delaunator::triangulate(
        pts.iter()
            .map(|p| delaunator::Point {
                x: p.x as f64,
                y: p.y as f64,
            })
            .collect::<Vec<_>>()
            .as_slice(),
    );

    let mut edges = HashSet::new();
    let mut it = del.triangles.iter();

    for _ in 0..del.len() {
        let a = *it.next().unwrap();
        let b = *it.next().unwrap();
        let c = *it.next().unwrap();
        edges.insert((a.min(b), a.max(b)));
        edges.insert((a.min(c), a.max(c)));
        edges.insert((b.min(c), b.max(c)));
    }

    edges
        .into_iter()
        .map(|(i, j)| (pts[i], pts[j]))
        .collect::<Vec<_>>()
}
