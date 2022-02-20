use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::ffi::c_void;
use std::mem;
use std::mem::transmute;
use std::sync::atomic::Ordering;
use std::time::Duration;

use gl::types::GLboolean;
use glutin::{Api, Context, ContextBuilder, ContextTrait, ElementState, Event, EventsLoop, GlRequest, MouseButton, VirtualKeyCode, WindowBuilder, WindowedContext, WindowEvent};
use glutin::dpi::LogicalSize;
use rand::Rng;

use scoundrel_common::glyphs::Glyph;
use scoundrel_common::keycodes::{KeyState, MouseState};
use crate::common::gl_error_check;

use crate::gl_state::GlState;
use scoundrel_common::presentation::Presentation;
use scoundrel_common::engine;

use crate::shader_pipeline::{QUAD_VERTEX_TEX_COORDS_COUNT, ShaderPipeline};
use crate::texture::Texture;

#[no_mangle]
pub static NvOptimusEnablement: u64 = 0x00000001;

#[no_mangle]
pub static AmdPowerXpressRequestHighPerformance: u64 = 0x00000001;

const RENDER_FRAME_DISTANCE: u64 = 20;

fn transmute_keycode(vk: VirtualKeyCode) -> KeyState {
    unsafe { std::mem::transmute(vk) }
}

fn transmute_mouse(mb: MouseButton, st: ElementState) -> MouseState {
    MouseState::new(unsafe { std::mem::transmute(mb) }, unsafe { std::mem::transmute(st) })
}

pub fn window_event_loop() {
    let mut event_loop = EventsLoop::new();

    let engine_options = engine::ENGINE_OPTIONS.lock().unwrap().clone();

    let window_builder = WindowBuilder::new()
        .with_title(engine_options.title.as_str())
        .with_resizable(false)
        .with_dimensions(LogicalSize::new(engine_options.window_size.0 as f64, engine_options.window_size.1 as f64,));

    let gl_context = glutin::ContextBuilder::new()
        .with_gl(GlRequest::Specific(Api::OpenGl, (4, 4)))
        .with_vsync(false)
        .build_windowed(window_builder, &event_loop).unwrap();

    let (pipeline, gl_state) = render_prepare(&gl_context);

    let mut done = false;
    while !done {
        event_loop.poll_events(|event| {
            match event {
                Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => {
                    engine::force_quit();

                    done = true;
                    println!("|= Quitting!");
                },

                Event::WindowEvent { event: WindowEvent::KeyboardInput { input, .. }, .. } => {
                    if let Some(key) = input.virtual_keycode {
                        engine::KEYBOARD_EVENTS.lock().unwrap().push_back(transmute_keycode(key));
                    }
                },

                Event::WindowEvent { event: WindowEvent::MouseInput{ button, state, .. }, .. } => {
                    engine::MOUSE_EVENTS.lock().unwrap().push_back(transmute_mouse(button, state));
                },

                Event::WindowEvent { event: WindowEvent::CursorMoved { position, ..}, .. } => {
                    let mut xy = engine::MOUSE_POSITIONS.lock().unwrap();
                    xy.borrow_mut().x = position.x as i32 / 16 as i32;
                    xy.borrow_mut().y = position.y as i32 / 16 as i32;
                },

                _ => {}
            }
        });

        render_frame(&gl_context, &pipeline);
    }
}

#[inline(always)]
fn render_prepare(gl_context: &WindowedContext) -> (ShaderPipeline, GlState) {
    unsafe { gl_context.make_current().unwrap() };

    let presentation = engine::ENGINE_OPTIONS.lock().unwrap().presentation.clone();
    println!("Preparing to use presentation: {}", presentation);

    let preferred_render_options = {
        let presentations = engine::PRESENTATIONS.lock().unwrap();
        presentations.get(presentation.as_str()).unwrap().clone()
    };

    gl::load_with(|symbol| gl_context.get_proc_address(symbol) as *const c_void);
    gl_error_check();

    let size = gl_context.window().get_inner_size().unwrap();
    let pipeline = ShaderPipeline::new(size, &preferred_render_options);

    let gl_state = unsafe {
        gl::UseProgram(pipeline.id);
        gl::BindVertexArray(pipeline.vao);

        gl::Viewport(0, 0, size.width as _, size.height as _);

        let mut texture = Texture::load(&preferred_render_options).unwrap();
        texture.bind();

        use nalgebra_glm::{ identity, ortho, translation, vec3, scaling, look_at };
        let projection = ortho(0.0f32, size.width as f32, size.height as f32, 0.0f32, 0.0f32, 100.0f32);
        let viewport = translation(&vec3(0.0f32, 0.0f32, 0.0f32));

        let camera = {
            let scale_matrix = scaling(&vec3(1.0, 1.0, 1.0));
            scale_matrix * look_at(&vec3(0.0f32, 0.0f32, 0.0f32), &(vec3(0.0f32, 0.0f32, -90.0f32)), &vec3(0.0f32, 1.0f32, 0.0f32))
        };

        GlState { texture, projection, viewport, camera }
    };

    let uniforms = pipeline.get_uniforms();
    unsafe {
        gl::UniformMatrix4fv(uniforms.projection, 1, false as GLboolean, nalgebra_glm::value_ptr(&gl_state.projection).as_ptr());
        gl_error_check();
        gl::UniformMatrix4fv(uniforms.viewport, 1, false as GLboolean, nalgebra_glm::value_ptr(&gl_state.viewport).as_ptr());
        gl_error_check();
        gl::UniformMatrix4fv(uniforms.camera, 1, false as GLboolean, nalgebra_glm::value_ptr(&gl_state.camera).as_ptr());
        gl_error_check();

        gl::Uniform2f(uniforms.input_font_bitmap_size, preferred_render_options.input_font_bitmap_size.0 as f32, preferred_render_options.input_font_bitmap_size.1 as f32);
        gl_error_check();
        gl::Uniform2f(uniforms.input_font_glyph_size, preferred_render_options.input_font_glyph_size.0 as f32, preferred_render_options.input_font_glyph_size.1 as f32);
        gl_error_check();
        gl::Uniform2f(uniforms.output_glyph_scale, preferred_render_options.output_glyph_scale.0 as f32, preferred_render_options.output_glyph_scale.1 as f32);
        gl_error_check();
        gl::Uniform2f(uniforms.window_size, size.width as f32, size.height as f32);
        gl_error_check();

        println!("font bitmap size:      {}, {}", preferred_render_options.input_font_bitmap_size.0 as f32, preferred_render_options.input_font_bitmap_size.1 as f32);
        println!("input font glyph size: {}, {}", preferred_render_options.input_font_glyph_size.0 as f32, preferred_render_options.input_font_glyph_size.1 as f32);
        println!("output glyph scale:    {}, {}", preferred_render_options.output_glyph_scale.0 as f32, preferred_render_options.output_glyph_scale.1 as f32);
        println!("window size:           {}, {}", size.width as f32, size.height as f32);
    }

    (pipeline, gl_state)
}

#[inline(always)]
fn render_frame(gl_context: &WindowedContext,
                pipeline: &ShaderPipeline,) {

    if engine::should_redraw() {
        let screen = engine::SCREEN.read().unwrap().clone();
        if screen.is_ready() {
            let glyphs = screen.glyphs();
            engine::clean_redraw();

            unsafe {
                use gl::types::GLsizeiptr;

                gl::BindBuffer(gl::ARRAY_BUFFER, pipeline.instance_glyphs_vbo);

                gl::BufferData(gl::ARRAY_BUFFER, mem::size_of_val(glyphs.as_slice()) as GLsizeiptr,
                               std::ptr::null(), gl::STREAM_DRAW, );

                gl::BufferData(gl::ARRAY_BUFFER, mem::size_of_val(glyphs.as_slice()) as GLsizeiptr,
                               glyphs.as_slice().as_ptr().cast(), gl::STREAM_DRAW, );

                gl::ClearColor(0.0, 0.0, 0.0, 1.0);
                gl_error_check();
                gl::Clear(gl::COLOR_BUFFER_BIT);
                gl_error_check();
                gl::DrawArraysInstanced(gl::TRIANGLES, 0,
                                        QUAD_VERTEX_TEX_COORDS_COUNT as _,
                                        glyphs.len() as i32);
                gl_error_check();
            }

            gl_context.swap_buffers().unwrap();
        }
    } else {
        std::thread::sleep(Duration::from_millis(1));
    }

    engine::FRAME_COUNTER.lock().unwrap().tick();
}