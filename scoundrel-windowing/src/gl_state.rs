use gl::types::GLuint;
use nalgebra_glm::TMat4;

pub struct GlState {
    pub framebuffer: GLuint,
    pub texture: GLuint,
    pub projection: TMat4<f32>,
    pub viewport: TMat4<f32>,
    pub camera: TMat4<f32>,
}