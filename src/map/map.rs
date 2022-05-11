use crate::map::Field;
use crate::map::StencilImpl;
use crate::Glyph;
use petgraph::Graph;

#[derive(Default)]
pub struct MapLevelComponent {
    pub walkable: Field<bool>,
    pub obscuring: Field<bool>,
    pub stage: Field<Glyph>,
    pub room_connectors: Vec<(u16, u16)>,
}
