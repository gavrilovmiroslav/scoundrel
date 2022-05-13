use crate::map::*;
use crate::{distance, Point};
use crate::{rand, Glyph};
use petgraph::algo::min_spanning_tree;
use petgraph::data::FromElements;
use std::collections::HashSet;
use std::ops::Range;

pub type Graph = petgraph::Graph<Point, f32>;
pub type Edge = (Point, Point);

pub fn minimum_spanning_tree(graph: Graph) -> HashSet<Edge> {
    let min_span = Graph::from_elements(min_spanning_tree(&graph));
    let mut return_set = HashSet::new();

    let edges = min_span.edge_indices().into_iter();
    for edge in edges {
        let w = min_span.edge_weight(edge).unwrap();
        if let Some((a, b)) = min_span.edge_endpoints(edge) {
            return_set.insert((
                min_span.node_weight(a).unwrap().clone(),
                min_span.node_weight(b).unwrap().clone(),
            ));
        }
    }

    return_set
}

pub fn triangulate_point_set(pts: &[Point]) -> Graph {
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

    let mut graph = Graph::new();

    let node_indices = pts
        .iter()
        .map(|pt| graph.add_node(pt.clone()))
        .collect::<Vec<_>>();

    for (a, b) in edges {
        graph.add_edge(node_indices[a], node_indices[b], distance(pts[a], pts[b]));
    }

    graph
}

pub fn get_all_edges(graph: &Graph) -> HashSet<Edge> {
    graph
        .edge_indices()
        .into_iter()
        .map(|e| {
            let (a, b) = graph.edge_endpoints(e).unwrap();
            (
                graph.node_weight(a).unwrap().clone(),
                graph.node_weight(b).unwrap().clone(),
            )
        })
        .collect()
}

pub fn construct_mesh(pts: &[Point]) -> (HashSet<Edge>, HashSet<Edge>) {
    let graph = triangulate_point_set(pts);
    let all_edges = get_all_edges(&graph);
    let trunk = minimum_spanning_tree(graph);
    let shrub = all_edges
        .difference(&trunk)
        .map(|x| x.clone())
        .collect::<HashSet<_>>();

    (trunk, shrub)
}

pub struct RoomStampDistribution {
    pub width: Range<u16>,
    pub height: Range<u16>,
    pub max_count: u16,
    pub min_distance: u16,
}

pub struct Percentage(u8);

impl Percentage {
    pub fn new(n: u8) -> Self {
        assert!(n <= 100, "Percentage are limited between 0 and 100");
        Percentage { 0: n }
    }
}

pub struct CorridorStampDistribution<'a> {
    pub glyph: Glyph,
    pub probability: Percentage,
    pub filter_lengths: Option<Range<u16>>,
    pub corridor_style: &'a dyn Fn(Point, Point) -> StencilImpl,
    pub ignore_already_connected: bool,
}

pub fn stamp_rooms(distrib: RoomStampDistribution, level: &mut MapLevelComponent) -> usize {
    let mut successful = 0;

    for _ in 1..distrib.max_count {
        let w = rand::u16(distrib.width.clone());
        let h = rand::u16(distrib.height.clone());
        let x = rand::i16(1..(level.size.0 as u16 - w) as i16);
        let y = rand::i16(1..(level.size.1 as u16 - h) as i16);

        let room = rect(x, y, w, h);
        let outside = grow(&room, distrib.min_distance);

        if level.walkable.overlaps(&outside) {
            continue;
        }

        level.put_room(&room, Glyph::FLOOR);
        successful += 1;
    }

    successful
}

pub fn stamp_corridors(
    distrib: CorridorStampDistribution,
    pts: &HashSet<Edge>,
    level: &mut MapLevelComponent,
) {
    let compare_probability = if distrib.probability.0 == 100 {
        None
    } else {
        let rng = {
            let len = pts.len();
            let mut rng = (0..len).collect::<Vec<_>>();
            rand::shuffle(&mut rng);
            let max = (len as f32 * distrib.probability.0 as f32 / 100.0f32).round() as usize;
            rng[0..max]
                .iter()
                .map(|x| x.clone())
                .collect::<HashSet<_>>()
        };
        Some(rng)
    };

    let mut index = 0usize;
    for (a, b) in pts {
        let mut skip = false;
        if let Some(rng) = &compare_probability {
            if !rng.contains(&index) {
                skip = true;
            }
        }
        if distrib.ignore_already_connected && level.is_connected(*a, *b) {
            skip = true;
        }
        if rand::u8(0..100) > distrib.probability.0 {
            skip = true;
        }

        if !skip {
            if let Some(filter) = &distrib.filter_lengths {
                let (door_left, door_right, d) = level.corridor_between(*a, *b);
                if filter.clone().contains(&(d.round() as u16)) {
                    level.put_corridor(
                        door_left,
                        door_right,
                        distrib.corridor_style,
                        distrib.glyph,
                    );
                }
            } else {
                level.put_corridor(*a, *b, distrib.corridor_style, distrib.glyph);
            }
        }

        index += 1;
    }
}
