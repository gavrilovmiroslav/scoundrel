use gl::types::GLuint;
use nalgebra_glm::TMat4;

use crate::texture::Texture;

pub struct GlState {
    pub texture: Texture,
    pub viewport: TMat4<f32>,
    pub projection: TMat4<f32>,
    pub camera: TMat4<f32>,
}