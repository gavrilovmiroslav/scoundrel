use std::mem::size_of;
use std::{ffi::CString, mem, ptr, str};

use gl::types::*;
use nalgebra_glm::TMat4;

use crate::core::engine::ENGINE_STATE;
use crate::core::NativeGlyph;
use crate::core::Presentation;

use crate::graphics::attribute::{AttribPosition, AttribSize, AttribType, BufferMapping};
use crate::graphics::common::gl_error_check;
use crate::graphics::texture::Texture;
use crate::graphics::uniforms::Uniforms;

pub const QUAD_VERTEX_TEX_COORDS_COUNT: usize = 24;
pub const QUAD_VERTEX_AND_TEX_COORDS: [f32; QUAD_VERTEX_TEX_COORDS_COUNT] = [
    -0.5, -0.5, 0.0, 0.0, -0.5, 0.5, 0.0, 1.0, 0.5, 0.5, 1.0, 1.0, 0.5, -0.5, 1.0, 0.0, -0.5, -0.5,
    0.0, 0.0, 0.5, 0.5, 1.0, 1.0,
];

pub const GLYPH_SIZE: usize = size_of::<NativeGlyph>();

const VERTEX: &str = include_str!("shaders/vertex_shader.glsl");
const FRAGMENT: &str = include_str!("shaders/fragment_shader.glsl");

fn compile_shader(src: &str, ty: GLenum) -> GLuint {
    unsafe {
        let shader = gl::CreateShader(ty);
        if shader == 0 {
            panic!("gl::CreateShader failed");
        }

        let src_ptr: *const _ = &src;
        gl::ShaderSource(shader, 1, src_ptr.cast(), &(src.len() as GLint));

        gl::CompileShader(shader);
        gl_error_check();

        let mut status = GLint::from(gl::FALSE);
        gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);

        if status != GLint::from(gl::TRUE) {
            let mut len = 0;
            gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf: Vec<u8> = Vec::with_capacity(len as usize);
            gl::GetShaderInfoLog(shader, len, ptr::null_mut(), buf.as_mut_ptr().cast());
            buf.set_len((len as usize) - 1);
        }

        shader
    }
}

fn compile_program(vertex: &str, fragment: &str) -> GLuint {
    let vs = compile_shader(vertex, gl::VERTEX_SHADER);
    let fs = compile_shader(fragment, gl::FRAGMENT_SHADER);
    unsafe {
        let program = gl::CreateProgram();
        if program == 0 {
            panic!("gl::CreateShader failed");
        }

        gl::AttachShader(program, vs);
        gl::AttachShader(program, fs);

        gl::LinkProgram(program);
        gl_error_check();

        let mut status = GLint::from(gl::FALSE);
        gl::GetProgramiv(program, gl::LINK_STATUS, &mut status);

        if status != GLint::from(gl::TRUE) {
            let mut len: GLint = 0;

            gl::GetProgramiv(program, gl::INFO_LOG_LENGTH, &mut len);
            let mut buf = Vec::with_capacity(len as usize);

            gl::GetProgramInfoLog(
                program,
                len,
                ptr::null_mut(),
                buf.as_mut_ptr() as *mut GLchar,
            );

            buf.set_len(len as usize - 1);
            panic!(
                "{}",
                str::from_utf8(&buf).expect("ProgramInfoLog not valid utf8")
            );
        }

        gl::DetachShader(program, fs);
        gl::DeleteShader(fs);
        gl::DetachShader(program, vs);
        gl::DeleteShader(vs);
        gl::UseProgram(program);
        gl_error_check();

        let color_str = CString::new("color").unwrap();
        gl::BindFragDataLocation(program, 0, color_str.as_ptr());
        program
    }
}

pub struct GlyphRenderer {
    pub shader: GLuint,
    pub vao: GLuint,
    pub glyph_buffer_size: usize,
    pub instance_glyphs_vbo: GLuint,
    pub texture: Texture,
    pub viewport: TMat4<f32>, // TODO: (low prio) these three could go somewhere else, to be reused
    pub projection: TMat4<f32>,
    pub camera: TMat4<f32>,
    static_quad_vbo: GLuint, // TODO: (low prio) move outside so that multiple layers could use it
}

impl GlyphRenderer {
    pub fn new(window_size: (u32, u32), pres: &Presentation) -> ((u32, u32), GlyphRenderer) {
        let id = compile_program(VERTEX, FRAGMENT);
        let mut vao = 0;
        let mut static_quad_vbo = 0;
        let mut instance_glyphs_vbo = 0;

        let scale = (
            pres.input_font_glyph_size.0 * pres.output_glyph_scale.0,
            pres.input_font_glyph_size.1 * pres.output_glyph_scale.1,
        );

        let glyph_count_by_width = window_size.0 as i32 / scale.0 as i32;
        let glyph_count_by_height = window_size.1 as i32 / scale.1 as i32;

        let buffer_size = glyph_count_by_width as usize * glyph_count_by_height as usize;
        let glyph_buffer_size = GLYPH_SIZE * buffer_size;

        unsafe {
            gl::GenVertexArrays(1, &mut vao);
            gl::BindVertexArray(vao);
        };

        unsafe {
            gl::GenBuffers(1, &mut static_quad_vbo);

            gl::BindBuffer(gl::ARRAY_BUFFER, static_quad_vbo);
            gl::BufferData(
                gl::ARRAY_BUFFER,
                (mem::size_of::<f32>() * QUAD_VERTEX_AND_TEX_COORDS.len() as usize) as GLsizeiptr,
                QUAD_VERTEX_AND_TEX_COORDS.as_ptr().cast(),
                gl::STATIC_DRAW,
            );

            BufferMapping::new_static()
                .with_attrib(
                    "vertex position",
                    AttribPosition(0),
                    AttribSize::Two,
                    AttribType::Float,
                )
                .with_attrib(
                    "vertex tex coord",
                    AttribPosition(1),
                    AttribSize::Two,
                    AttribType::Float,
                )
                .build_bound_buffer(0);
        }

        unsafe {
            gl::GenBuffers(1, &mut instance_glyphs_vbo);
            gl::BindBuffer(gl::ARRAY_BUFFER, instance_glyphs_vbo);

            let screen = &mut ENGINE_STATE.lock().unwrap().render_state.screen;

            let mut glyphs = Vec::new();
            for _ in 0..buffer_size {
                glyphs.push(NativeGlyph::default())
            }

            screen.set_memory(
                (glyph_count_by_width as u32, glyph_count_by_height as u32),
                glyphs,
            );

            gl::BufferData(
                gl::ARRAY_BUFFER,
                (screen.len() * std::mem::size_of::<NativeGlyph>()) as GLsizeiptr,
                screen.get_memory().cast(),
                gl::STREAM_DRAW,
            );

            BufferMapping::new_instanced()
                .with_attrib(
                    "glyph symbol",
                    AttribPosition(2),
                    AttribSize::One,
                    AttribType::UnsignedInt,
                )
                .with_attrib(
                    "glyph foreground",
                    AttribPosition(3),
                    AttribSize::One,
                    AttribType::UnsignedInt,
                )
                .with_attrib(
                    "glyph background",
                    AttribPosition(4),
                    AttribSize::One,
                    AttribType::UnsignedInt,
                )
                .build_bound_buffer(0);
        }
        gl_error_check();

        let (texture, projection, viewport, camera) = unsafe {
            gl::UseProgram(id);
            gl::BindVertexArray(vao);

            gl::Viewport(0, 0, window_size.0 as _, window_size.1 as _);

            let texture = Texture::load(&pres).unwrap();
            texture.bind();

            use nalgebra_glm::{look_at, ortho, scaling, translation, vec3};
            let projection = ortho(
                0.0f32,
                window_size.0 as f32,
                window_size.1 as f32,
                0.0f32,
                0.0f32,
                100.0f32,
            );
            let viewport = translation(&vec3(0.0f32, 0.0f32, 0.0f32));

            let camera = {
                let scale_matrix = scaling(&vec3(1.0, 1.0, 1.0));
                scale_matrix
                    * look_at(
                        &vec3(0.0f32, 0.0f32, 0.0f32),
                        &(vec3(0.0f32, 0.0f32, -90.0f32)),
                        &vec3(0.0f32, 1.0f32, 0.0f32),
                    )
            };

            texture.unbind();

            (texture, projection, viewport, camera)
        };

        let renderer = GlyphRenderer {
            shader: id,
            vao,
            static_quad_vbo,
            instance_glyphs_vbo,
            glyph_buffer_size,
            texture,
            projection,
            viewport,
            camera,
        };

        (pres.window_size_in_glyphs, renderer)
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

    pub fn render(&self, glyphs: &Vec<NativeGlyph>) {
        self.bind();
        unsafe {
            gl::BindBuffer(gl::ARRAY_BUFFER, self.instance_glyphs_vbo);

            gl::BufferData(
                gl::ARRAY_BUFFER,
                mem::size_of_val(glyphs.as_slice()) as GLsizeiptr,
                std::ptr::null(),
                gl::STREAM_DRAW,
            );

            gl::BufferData(
                gl::ARRAY_BUFFER,
                mem::size_of_val(glyphs.as_slice()) as GLsizeiptr,
                glyphs.as_slice().as_ptr().cast(),
                gl::STREAM_DRAW,
            );

            gl::DrawArraysInstanced(
                gl::TRIANGLES,
                0,
                QUAD_VERTEX_TEX_COORDS_COUNT as _,
                glyphs.len() as i32,
            );
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
