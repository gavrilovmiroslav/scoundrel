use std::{
    ffi::{c_void, CString},
    mem, ptr, str,
};
use std::mem::size_of;

use gl::types::*;
use glutin::dpi::LogicalSize;
use glutin::WindowedContext;
use nalgebra_glm::TMat4;
use rand::{Rng, thread_rng};

use scoundrel_common::colors::Color;
use scoundrel_common::glyphs::Glyph;

use crate::common::gl_error_check;
use scoundrel_common::presentation::Presentation;
use crate::attribute::{BufferMapping, AttribPosition, AttribSize, AttribType};
use crate::shader_pipeline::VertexBufferInitProfile::{Data, Size};
use crate::uniforms::Uniforms;
use scoundrel_common::engine;
use crate::texture::Texture;

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

pub struct GlyphRenderer {
    pub shader: GLuint,
    pub vao: GLuint,
    pub glyph_buffer_size: usize,
    pub instance_glyphs_vbo: GLuint,
    pub texture: Texture,
    pub viewport: TMat4<f32>,   // TODO: these three could go somewhere else, to be reused
    pub projection: TMat4<f32>,
    pub camera: TMat4<f32>,
    static_quad_vbo: GLuint, // TODO: move outside so that multiple layers could use it
}

impl GlyphRenderer {
    pub fn new(window_size: LogicalSize, render_options: &Presentation) -> GlyphRenderer {
        let id = compile_program(VERTEX, FRAGMENT);
        let mut vao = 0;
        let mut static_quad_vbo = 0;
        let mut instance_glyphs_vbo = 0;
        let mut glyph_buffer_size = 0;

        let scale = (render_options.input_font_glyph_size.0 * render_options.output_glyph_scale.0,
                     render_options.input_font_glyph_size.1 * render_options.output_glyph_scale.1);

        let glyph_count_by_width = window_size.width as i32 / scale.0 as i32;
        let glyph_count_by_height = window_size.height as i32 / scale.1 as i32;

        let buffer_size = glyph_count_by_width as usize * glyph_count_by_height as usize;
        glyph_buffer_size = GLYPH_SIZE * buffer_size;

        unsafe {
            gl::GenVertexArrays(1, &mut vao);
            gl::BindVertexArray(vao);
        };

        unsafe {
            gl::GenBuffers(1, &mut static_quad_vbo);

            gl::BindBuffer(gl::ARRAY_BUFFER, static_quad_vbo);
            gl::BufferData(gl::ARRAY_BUFFER, (mem::size_of::<f32>() * QUAD_VERTEX_AND_TEX_COORDS.len() as usize) as GLsizeiptr,
                           QUAD_VERTEX_AND_TEX_COORDS.as_ptr().cast(), gl::STATIC_DRAW, );

            BufferMapping::new_static()
                .with_attrib("vertex position", AttribPosition(0), AttribSize::Two, AttribType::Float)
                .with_attrib("vertex tex coord", AttribPosition(1), AttribSize::Two, AttribType::Float)
                .build_bound_buffer(0);
        }

        unsafe {
            gl::GenBuffers(1, &mut instance_glyphs_vbo);
            gl::BindBuffer(gl::ARRAY_BUFFER, instance_glyphs_vbo);

            let mut screen = engine::SCREEN.write().unwrap();

            let mut glyphs = Vec::new();
            for _ in 0..buffer_size {
                glyphs.push(Glyph::default())
            }

            screen.set_memory((glyph_count_by_width as u32, glyph_count_by_height as u32), glyphs);

            gl::BufferData(gl::ARRAY_BUFFER, (screen.len() * std::mem::size_of::<Glyph>()) as GLsizeiptr,
                           screen.get_memory().cast(), gl::STREAM_DRAW, );

            BufferMapping::new_instanced()
                .with_attrib("glyph symbol", AttribPosition(2), AttribSize::One, AttribType::UnsignedInt)
                .with_attrib("glyph foreground", AttribPosition(3), AttribSize::One, AttribType::UnsignedInt)
                .with_attrib("glyph background", AttribPosition(4), AttribSize::One, AttribType::UnsignedInt)
                .build_bound_buffer(0);
        }
        gl_error_check();

        let (texture, projection, viewport, camera) = unsafe {
            gl::UseProgram(id);
            gl::BindVertexArray(vao);

            gl::Viewport(0, 0, window_size.width as _, window_size.height as _);

            let mut texture = Texture::load(&render_options).unwrap();
            texture.bind();

            use nalgebra_glm::{identity, ortho, translation, vec3, scaling, look_at};
            let projection = ortho(0.0f32, window_size.width as f32, window_size.height as f32, 0.0f32, 0.0f32, 100.0f32);
            let viewport = translation(&vec3(0.0f32, 0.0f32, 0.0f32));

            let camera = {
                let scale_matrix = scaling(&vec3(1.0, 1.0, 1.0));
                scale_matrix * look_at(&vec3(0.0f32, 0.0f32, 0.0f32), &(vec3(0.0f32, 0.0f32, -90.0f32)), &vec3(0.0f32, 1.0f32, 0.0f32))
            };

            texture.unbind();

            (texture, projection, viewport, camera)
        };

        GlyphRenderer { shader: id, vao, static_quad_vbo, instance_glyphs_vbo, glyph_buffer_size, texture, projection, viewport, camera }
    }

    fn bind(&self) {
        unsafe {
            gl::BindVertexArray(self.vao);
            self.texture.bind();
        };
    }

    fn unbind(&self) {
        unsafe {
            gl::BindVertexArray(0);
            self.texture.unbind();
        };
    }

    pub fn render(&self, glyphs: &Vec<Glyph>) {
        self.bind();
        unsafe {
            use gl::types::GLsizeiptr;

            gl::BindBuffer(gl::ARRAY_BUFFER, self.instance_glyphs_vbo);

            gl::BufferData(gl::ARRAY_BUFFER, mem::size_of_val(glyphs.as_slice()) as GLsizeiptr,
                           std::ptr::null(), gl::STREAM_DRAW, );

            gl::BufferData(gl::ARRAY_BUFFER, mem::size_of_val(glyphs.as_slice()) as GLsizeiptr,
                           glyphs.as_slice().as_ptr().cast(), gl::STREAM_DRAW, );

            gl::DrawArraysInstanced(gl::TRIANGLES, 0,
                                    QUAD_VERTEX_TEX_COORDS_COUNT as _,
                                    glyphs.len() as i32);
        }
        self.unbind();
    }
    pub fn get_uniforms(&self) -> Uniforms {
        Uniforms::fetch_for_program(self.shader)
    }
}

impl Drop for GlyphRenderer {
    fn drop(&mut self) {
        unsafe {
            gl::DeleteProgram(self.shader);
            gl::DeleteBuffers(1, &self.static_quad_vbo);
            gl::DeleteBuffers(1, &self.instance_glyphs_vbo);
            gl::DeleteVertexArrays(1, &self.vao);
        }
    }
}
