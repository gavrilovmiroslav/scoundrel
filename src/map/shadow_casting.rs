use std::collections::HashMap;
use std::ops::Range;
use crate::Point;

pub trait Shadowcaster {
    fn compute(origin: Point, max_depth: u16, is_blocking: &impl Fn(&Point) -> bool, mark: &mut HashMap<Point, bool>);
}

#[allow(dead_code)]
pub fn round_towards_zero(n: f32) -> i16 {
    (n.signum() * (n.abs() - 0.5).ceil()) as i16
}

pub fn round_towards_pos_inf(n: f32) -> i16 {
    (n + 0.5).floor() as i16
}

pub fn round_towards_neg_inf(n: f32) -> i16 {
    (n - 0.5).ceil() as i16
}

pub enum QuadrantDirection {
    North,
    East,
    South,
    West,
}

impl QuadrantDirection {
    pub fn all() -> [QuadrantDirection;4] {
        use QuadrantDirection::*;
        [ North, East, South, West ]
    }
}

pub struct Quadrant {
    direction: QuadrantDirection,
    origin: Point,
}

impl Quadrant {
    pub fn new(direction: QuadrantDirection, origin: Point) -> Self {
        Self { direction, origin }
    }

    pub fn transform(&self, tile: &Point) -> Option<Point> {
        let (row, col) = (tile.x, tile.y);
        let diff = match self.direction {
            QuadrantDirection::North => (col, -row),
            QuadrantDirection::East => (col, row),
            QuadrantDirection::South => (row, col),
            QuadrantDirection::West => (-row, col),
        };
        let next = Point::from(diff) + self.origin;

        if next.is_non_negative() {
            Some(next)
        } else {
            None
        }
    }
}

type Depth = u16;
type Slope = f32;

#[derive(Debug)]
pub struct Row {
    depth: Depth,
    start_slope: Slope,
    end_slope: Slope,
}

impl Row {
    pub fn new(depth: Depth, start_slope: Slope, end_slope: Slope) -> Self {
        Self { depth, start_slope, end_slope }
    }

    pub fn tiles(&self) -> (Depth, Range<i16>) {
        let min_col = round_towards_pos_inf(self.depth as f32 * self.start_slope);
        let max_col = round_towards_neg_inf(self.depth as f32 * self.end_slope) + 1;
        (self.depth, min_col..max_col)
    }

    pub fn next(&self) -> Row {
        Row::new(self.depth + 1, self.start_slope, self.end_slope)
    }
}

pub struct SymmetricShadowcast;

impl SymmetricShadowcast {
    pub fn slope(tile: &Point) -> f32 {
        let a = tile.y as f32 - 0.5;
        let b = tile.x as f32;
        a / b
    }

    pub fn is_symmetric(row: &Row, tile: &Point) -> bool {
        let col = tile.y as f32;
        (col >= row.depth as f32 * row.start_slope) && (col <= row.depth as f32 * row.end_slope)
    }

    // is wall
    pub fn is_blocking_reorient(tile: &Point, quad: &Quadrant, is_blocking: &impl Fn(&Point) -> bool) -> bool {
        if let Some(pt) = quad.transform(tile) {
            is_blocking(&pt)
        } else {
            false
        }
    }

    // is floor
    pub fn is_walkable_reorient(tile: &Point, quad: &Quadrant, is_blocking: &impl Fn(&Point) -> bool) -> bool {
        if let Some(pt) = quad.transform(tile) {
            !is_blocking(&pt)
        } else {
            false
        }
    }

    pub fn reveal_reoriented(tile: &Point, quad: &Quadrant, mark: &mut HashMap<Point, bool>) {
        if let Some(pt) = quad.transform(tile) {
            mark.insert(pt, true);
        }
    }

    pub fn conceal_reoriented(tile: &Point, quad: &Quadrant, mark: &mut HashMap<Point, bool>) {
        if let Some(pt) = quad.transform(tile) {
            mark.insert(pt, false);
        }
    }

    pub fn scan(row: Row, max_depth: u16, quad: &Quadrant,
                is_blocking: &impl Fn(&Point) -> bool,
                mark: &mut HashMap<Point, bool>) {

        let mut rows = Vec::new();
        rows.push(row);

        while !rows.is_empty() {
            let mut row = rows.pop().unwrap();
            let mut prev_tile = None;
            let (depth, range) = row.tiles();

            if depth > max_depth { continue; }

            for tile in range.map(|c| Point::from((depth as i16, c))) {
                if Self::is_blocking_reorient(&tile, quad, &is_blocking) ||
                    Self::is_symmetric(&row, &tile) {

                    Self::reveal_reoriented(&tile, quad, mark);
                }

                if let Some(prev) = prev_tile {
                    if Self::is_blocking_reorient(&prev, quad, &is_blocking) &&
                        Self::is_walkable_reorient(&tile, quad, &is_blocking) {

                        Self::conceal_reoriented(&prev, quad, mark);
                        row.start_slope = Self::slope(&tile);
                    }

                    if Self::is_walkable_reorient(&prev, quad, &is_blocking) &&
                        Self::is_blocking_reorient(&tile, quad, &is_blocking) {

                        Self::conceal_reoriented(&tile, quad, mark);

                        let mut next_row = row.next();
                        next_row.end_slope = Self::slope(&tile);
                        rows.push(next_row);
                    }
                }

                prev_tile = Some(tile);
            }

            if prev_tile.is_some() && Self::is_walkable_reorient(prev_tile.as_ref().unwrap(), quad, &is_blocking) {
                rows.push(row.next());
            }
        }
    }
}

impl Shadowcaster for SymmetricShadowcast {
    fn compute(origin: Point, max_depth: u16, is_blocking: &impl Fn(&Point) -> bool, mark: &mut HashMap<Point, bool>) {
        mark.insert(origin, true);

        for dir in QuadrantDirection::all() {
            let quad = Quadrant::new(dir, origin);
            let row = Row::new(1, -1.0, 1.0);
            SymmetricShadowcast::scan(row, max_depth, &quad, &is_blocking, mark);
        }
    }
}
