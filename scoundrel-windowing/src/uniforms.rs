use std::ffi::CString;

use gl::types::{GLfloat, GLint, GLuint};

use crate::common::gl_error_check;

fn get_uniform_id(program: GLuint, name_str: &str) -> GLint {
    let name = CString::new(name_str).unwrap();
    let id = unsafe { gl::GetUniformLocation(program, name.as_ptr()) };
    gl_error_check();

    if id == -1 {
        panic!("Unknown uniform in program {}: {}", program, name_str)
    } else {
        id
    }
}

type Uniform = GLint;

pub struct Uniforms {
    pub projection: GLint,
    pub viewport: GLint,
    pub camera: GLint,
    pub input_font_bitmap_size: GLint,
    pub input_font_glyph_size: GLint,
    pub output_glyph_scale: GLint,
    pub window_size: GLint,
}

impl Uniforms {
    pub fn fetch_for_program(id: GLuint) -> Uniforms {
        Uniforms{
            projection: get_uniform_id(id, "u_Projection"),
            viewport: get_uniform_id(id, "u_Viewport"),
            camera: get_uniform_id(id, "u_Camera"),
            input_font_bitmap_size: get_uniform_id(id, "u_InputFontBitmapSize"),
            input_font_glyph_size: get_uniform_id(id, "u_InputFontGlyphSize"),
            output_glyph_scale: get_uniform_id(id, "u_OutputGlyphScale"),
            window_size: get_uniform_id(id, "u_WindowSize"),
        }
    }
}