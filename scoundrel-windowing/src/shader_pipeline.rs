use std::{
    ffi::{c_void, CString},
    mem, ptr, str,
};
use std::mem::size_of;

use gl::types::*;
use glutin::dpi::LogicalSize;

use scoundrel_common::colors::Color;
use scoundrel_common::glyphs::Glyph;

use crate::common::gl_error_check;
use crate::shader_pipeline::VertexBufferInitProfile::{Data, Size};
use crate::uniforms::Uniforms;

pub struct Attribute {
    pub position: GLuint,
    pub stride: i32,
    pub pointer: usize,
}

pub const VERTEX_POSITION_ATTRIBUTE: Attribute = Attribute{ position: 0, stride: 2, pointer: 0 };
pub const VERTEX_TEX_COORD_ATTRIBUTE: Attribute = Attribute{ position: 1, stride: 2, pointer: 2 };

pub const GLYPH_SYMBOL_ATTRIBUTE: Attribute = Attribute{ position: 2, stride: 1, pointer: 0 };
pub const GLYPH_FOREGROUND_ATTRIBUTE: Attribute = Attribute{ position: 3, stride: 1, pointer: 1 };
pub const GLYPH_BACKGROUND_ATTRIBUTE: Attribute = Attribute{ position: 4, stride: 1, pointer: 2 };

pub const QUAD_VERTEX_TEX_COORDS_COUNT: usize = 24;
pub const QUAD_MEMORY_SIZE: usize = unsafe { size_of::<f32>() } * QUAD_VERTEX_TEX_COORDS_COUNT;
pub const QUAD_VERTEX_AND_TEX_COORDS: [f32; QUAD_VERTEX_TEX_COORDS_COUNT] = [
    -0.5, -0.5, 0.0, 0.0,
    -0.5,  0.5, 0.0, 1.0,
    0.5,  0.5, 1.0, 1.0,
    0.5, -0.5, 1.0, 0.0,
    -0.5, -0.5, 0.0, 0.0,
    0.5,  0.5, 1.0, 1.0
];

pub const GLYPH_SIZE: usize = unsafe { size_of::<Glyph>() };

const VERTEX: &str = include_str!("vertex_shader.glsl");
const FRAGMENT: &str = include_str!("fragment_shader.glsl");

fn compile_shader(src: &str, ty: GLenum) -> GLuint {
    unsafe {
        // SAFETY: `ty` is either `gl::VERTEX_SHADER` or `gl::FRAGMENT_SHADER`
        let shader = gl::CreateShader(ty);
        if shader == 0 {
            panic!("gl::CreateShader failed");
        }

        // SAFETY:
        // `shader` is a shader object created by OpenGL
        // `count` is one
        let src_ptr: *const _ = &src;
        gl::ShaderSource(shader, 1, src_ptr.cast(), &(src.len() as GLint));

        // SAFETY: `shader` is a shader object created by OpenGL
        gl::CompileShader(shader);
        gl_error_check();

        // Get the compile status
        let mut status = GLint::from(gl::FALSE);
        // SAFETY: `gl::COMPILE_STATUS` is a valid `pname`
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);

        // Fail on error
        if status != GLint::from(gl::TRUE) {
            let mut len = 0;
            // SAFETY: `gl::INFO_LENGTH` is a valid `pname`
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf: Vec<u8> = Vec::with_capacity(len as usize);
            // SAFETY: `maxLength` is the value of `gl::INFO_LOG_LENGTH`
            gl::GetShaderInfoLog(shader, len, ptr::null_mut(), buf.as_mut_ptr().cast());
            // SAFETY: the content has been written by `gl::GetShaderInfoLog`
            buf.set_len((len as usize) - 1);
        }

        shader
    }
}

fn compile_program(vertex: &str, fragment: &str) -> GLuint {
    let vs = compile_shader(vertex, gl::VERTEX_SHADER);
    let fs = compile_shader(fragment, gl::FRAGMENT_SHADER);
    unsafe {
        // SAFETY: can not fail
        let program = gl::CreateProgram();
        if program == 0 {
            panic!("gl::CreateShader failed");
        }

        // SAFETY:
        // `program` is a valid program object
        // `vs` and `fs` are both unused valid shader objects
        gl::AttachShader(program, vs);
        gl::AttachShader(program, fs);

        // SAFETY:
        // `program` is a valid program object and not active
        gl::LinkProgram(program);
        gl_error_check();

        // SAFETY:
        // `program` is a valid program object
        // `gl::LINK_STATUS` is a valid `pname`
        let mut status = GLint::from(gl::FALSE);
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut status);

        if status != GLint::from(gl::TRUE) {
            let mut len: GLint = 0;
            // SAFETY: `gl::COMPILE_STATUS` is a valid `pname`
            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);
            // SAFETY: `maxLength` is the value of `gl::INFO_LOG_LENGTH`
            gl::GetProgramInfoLog(
                program,
                len,
                ptr::null_mut(),
                buf.as_mut_ptr() as *mut GLchar,
            );
            // SAFETY: the content has been written by `gl::GetProgramInfoLog`
            buf.set_len(len as usize - 1);
            panic!("{}", str::from_utf8(&buf).expect("ProgramInfoLog not valid utf8"));
        }

        // SAFETY:
        // `program` is a valid program object
        // `fs` and `vs` are both valid shaders and attached to `program`
        gl::DetachShader(program, fs);
        gl::DeleteShader(fs);
        gl::DetachShader(program, vs);
        gl::DeleteShader(vs);
        gl::UseProgram(program);
        gl_error_check();

        // SAFETY: `colorNumber` is zero, which is less than `GL_MAX_DRAW_BUFFERS`
        let color_str = CString::new("color").unwrap();
        gl::BindFragDataLocation(program, 0, color_str.as_ptr());
        program
    }
}

pub enum VertexBufferInitProfile<'a> {
    Data(&'a [GLfloat]),
    Size(usize),
}

fn init_vertex_buffer(vbo: GLuint, data: VertexBufferInitProfile) {
    use VertexBufferInitProfile::*;

    unsafe {
        // SAFETY: `gl::ARRAY_BUFFER` is a valid `target` and `vbo` is valid
        gl::BindBuffer(gl::ARRAY_BUFFER, vbo);
        // SAFETY:
        // `gl::ARRAY_BUFFER` is a valid buffer target
        // `gl::STATIC_DRAW` is a valid usage
        // `size` is positive
        // `vbo` is bound to `target`
        // `GL_BUFFER_IMMUTABLE_STORAGE` is not yet set
        match data {
            Data(data) => {
                gl::BufferData(gl::ARRAY_BUFFER, mem::size_of_val(data) as GLsizeiptr,
                               data.as_ptr().cast(), gl::STATIC_DRAW, );
            }
            Size(size) => {
                gl::BufferData(gl::ARRAY_BUFFER, size as GLsizeiptr,
                               ptr::null(), gl::STATIC_DRAW, );
            }
        }

        // check for oom
        gl_error_check();
    }
}

#[derive(Debug)]
pub struct ShaderPipeline {
    pub id: GLuint,
    pub vao: GLuint,
    pub glyph_buffer_size: usize,
    static_quad_vbo: GLuint,
    instance_glyphs_vbo: GLuint,
}

fn set_attributes(size: i32, divisor: bool, attribs: &[Attribute]) {
    let float_size = unsafe { size_of::<f32>() } as i32;
    let byte_size = float_size * size;
    assert_eq!(attribs.clone().iter().fold(0, |acc, x| acc + x.stride * float_size), byte_size);

    for attrib in attribs {
        unsafe { gl::EnableVertexAttribArray(attrib.position); }
        unsafe {
            gl::VertexAttribPointer(
                attrib.position,
                attrib.stride,
                gl::FLOAT,
                gl::FALSE as GLboolean,
                byte_size,
                (size_of::<f32>() * attrib.pointer) as *const c_void,
            );

            if divisor {
                gl::VertexAttribDivisor(attrib.position, 1);
            }

            gl_error_check();
        }
    }
}

impl ShaderPipeline {
    pub fn new(window_size: LogicalSize, scale: f32) -> ShaderPipeline {
        let id = compile_program(VERTEX, FRAGMENT);
        let mut vao = 0;
        let mut static_quad_vbo = 0;
        let mut instance_glyphs_vbo = 0;
        let mut glyph_buffer_size = 0;

        unsafe {
            // SAFETY: `n` is positive
            gl::GenVertexArrays(1, &mut vao);
            gl::GenBuffers(1, &mut static_quad_vbo);
            gl::GenBuffers(1, &mut instance_glyphs_vbo);

            // SAFETY: `vao` was just returned from `gl::GenVertexArrays`
            gl::BindVertexArray(vao);

            init_vertex_buffer(static_quad_vbo, Data(&QUAD_VERTEX_AND_TEX_COORDS));
            set_attributes(4, false, &[
                VERTEX_POSITION_ATTRIBUTE,
                VERTEX_TEX_COORD_ATTRIBUTE,
            ]);

            let glyph_width = window_size.width as i32 / 16; //scale as i32;
            let glyph_height = window_size.height as i32 / 16; //scale as i32;
            glyph_buffer_size = GLYPH_SIZE * glyph_width as usize * glyph_height as usize;
            init_vertex_buffer(instance_glyphs_vbo, Size(glyph_buffer_size));
            set_attributes(3, true, &[
                GLYPH_SYMBOL_ATTRIBUTE,
                GLYPH_FOREGROUND_ATTRIBUTE,
                GLYPH_BACKGROUND_ATTRIBUTE,
            ]);
        }
        gl_error_check();

        ShaderPipeline { id, vao, static_quad_vbo, instance_glyphs_vbo, glyph_buffer_size  }
    }

    pub fn get_uniforms(&self) -> Uniforms {
        Uniforms::fetch_for_program(self.id)
    }
}

impl Drop for ShaderPipeline {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteProgram(self.id);
            gl::DeleteBuffers(1, &self.static_quad_vbo);
            gl::DeleteBuffers(1, &self.instance_glyphs_vbo);
            gl::DeleteVertexArrays(1, &self.vao);
        }
    }
}
