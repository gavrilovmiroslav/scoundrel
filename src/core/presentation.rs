use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Presentation {
    pub input_font: String,
    pub input_font_bitmap_size: (u32, u32),
    pub input_font_glyph_size: (u32, u32),
    pub output_glyph_scale: (u32, u32),
    pub window_size_in_glyphs: (u32, u32),
}
