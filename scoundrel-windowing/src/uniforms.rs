use std::ffi::CString;

use gl::types::{GLint, GLuint};

fn get_uniform_id(program: GLuint, name_str: &str) -> GLint {
    let name = CString::new(name_str).unwrap();
    let id = unsafe { gl::GetUniformLocation(program, name.as_ptr()) };

    if id == -1 {
        panic!("Unknown uniform in program {}: {}", program, name_str)
    } else {
        id
    }
}

pub struct Uniforms {
    pub projection: GLint,
    pub viewport: GLint,
    pub camera: GLint,
    pub window_size: GLint,
    pub glyph_size: GLint,
}

impl Uniforms {
    pub fn fetch_for_program(id: GLuint) -> Uniforms {
        Uniforms{
            projection: get_uniform_id(id, "u_Projection"),
            viewport: get_uniform_id(id, "u_Viewport"),
            camera: get_uniform_id(id, "u_Camera"),
            window_size: get_uniform_id(id, "u_WindowSize"),
            glyph_size: get_uniform_id(id, "u_GlyphSize"),
        }
    }
}