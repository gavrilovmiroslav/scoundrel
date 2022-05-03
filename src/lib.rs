mod core;
mod graphics;

pub use crate::core::colors;
pub use crate::core::engine::{force_quit, force_redraw, should_quit, should_redraw};
pub use crate::core::engine::{get_filename_when_changed, snoop_for_data_changes};
pub use crate::core::glyphs;
pub use crate::core::input;
pub use crate::core::point;
pub use crate::graphics::window::EngineInstance;

pub use crate::core::input::poll_inputs;
pub use crate::graphics::render;
