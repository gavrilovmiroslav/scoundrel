use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::ffi::c_void;
use std::mem::transmute;
use std::sync::atomic::Ordering;

use gl::types::GLboolean;
use glutin::{Api, Context, ContextBuilder, ContextTrait, ElementState, Event, EventsLoop, GlRequest, MouseButton, VirtualKeyCode, WindowBuilder, WindowedContext, WindowEvent};
use rand::Rng;

use scoundrel_common::engine_context::EngineContext;
use scoundrel_common::keycodes::{KeyState, MouseState};

use crate::gl_state::GlState;
use crate::shader_pipeline::{QUAD_VERTEX_TEX_COORDS_COUNT, ShaderPipeline};

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

pub fn window_event_loop(engine_context: EngineContext) {
    let mut event_loop = EventsLoop::new();

    let window_builder = WindowBuilder::new();
    let gl_context = glutin::ContextBuilder::new()
        .with_gl(GlRequest::Specific(Api::OpenGl, (4, 4)))
        .with_vsync(false)
        .build_windowed(window_builder, &event_loop).unwrap();

    let (pipeline, gl_state) = render_prepare(&gl_context, &engine_context);

/*
    let mut rng = rand::thread_rng();

    let texture = Texture::load(&mut context, "./textures/8x8glyphs.png").unwrap();
    let textures_by_char = {
        let mut map_index = HashMap::new();
        let mut index = 0u16;
        for i in 0..16 {
            for j in 0..16 {
                map_index.insert(index, texture.get_section((8 * j, texture.height() - 8 * (i + 1)), (8, 8)));
                index += 1;
            }
        }
        map_index
    };
*/

    let mut done = false;
    while !done {
        let mut frame = 0u64;
        event_loop.poll_events(|event| {
            match event {
                Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => {
                    engine_context.should_quit.store(true, Ordering::SeqCst);

                    done = true;
                    println!("|= Quitting!");
                },

                Event::WindowEvent { event: WindowEvent::KeyboardInput { input, .. }, .. } => {
                    if let Some(key) = input.virtual_keycode {
                        engine_context.keyboard_events.lock().unwrap().push_back(transmute_keycode(key));
                    }
                },

                Event::WindowEvent { event: WindowEvent::MouseInput{ button, state, .. }, .. } => {
                    engine_context.mouse_events.lock().unwrap().push_back(transmute_mouse(button, state));
                },

                Event::WindowEvent { event: WindowEvent::CursorMoved { position, ..}, .. } => {
                    let mut xy = engine_context.mouse_position.lock().unwrap();
                    xy.borrow_mut().x = position.x as i32 / engine_context.get_glyph_size() as i32;
                    xy.borrow_mut().y = position.y as i32 / engine_context.get_glyph_size() as i32;
                },

                _ => {}
            }
        });

        render_frame(&gl_context, &engine_context, &pipeline, &gl_state);

        frame += 1;
        *engine_context.frame_counter.write().unwrap() += 1;
    }
}

#[inline(always)]
fn render_prepare(gl_context: &WindowedContext, engine_context: &EngineContext) -> (ShaderPipeline, GlState) {
    unsafe { gl_context.make_current().unwrap() };
    gl::load_with(|symbol| gl_context.get_proc_address(symbol) as *const c_void);

    let scale =  engine_context.get_glyph_size() as f32;
    let pipeline = ShaderPipeline::new(gl_context.window().get_inner_size().unwrap(), scale);
    let size = gl_context.window().get_inner_size().unwrap();
    let gl_state = unsafe {
        gl::UseProgram(pipeline.id);
        gl::BindVertexArray(pipeline.vao);

        gl::Viewport(0, 0, size.width as _, size.height as _);

        let framebuffer = 0;
        gl::BindFramebuffer(gl::FRAMEBUFFER, framebuffer);

        let texture = 0;
        gl::BindTexture(gl::TEXTURE_2D, texture);
        assert_eq!(gl::NO_ERROR, gl::GetError());

        use nalgebra_glm::{ identity, ortho, translation, vec3, scaling, look_at };
        let projection = ortho(0.0f32, size.width as f32, size.height as f32, 0.0f32, 0.0f32, 100.0f32);
        let viewport = {
            let half_scale = scale * 0.5f32;
            translation(&vec3(half_scale, half_scale, 0.0f32))
        };

        let camera = {
            let scale_matrix = scaling(&vec3(scale, scale, scale));
            scale_matrix * look_at(&vec3(0.0f32, 0.0f32, 0.0f32), &(vec3(0.0f32, 0.0f32, -90.0f32)), &vec3(0.0f32, 1.0f32, 0.0f32))
        };

        GlState { framebuffer, texture, projection, viewport, camera }
    };

    let uniforms = pipeline.get_uniforms();
    unsafe {
        gl::UniformMatrix4fv(uniforms.projection, 1, false as GLboolean, nalgebra_glm::value_ptr(&gl_state.projection).as_ptr());
        gl::UniformMatrix4fv(uniforms.viewport, 1, false as GLboolean, nalgebra_glm::value_ptr(&gl_state.viewport).as_ptr());
        gl::UniformMatrix4fv(uniforms.camera, 1, false as GLboolean, nalgebra_glm::value_ptr(&gl_state.camera).as_ptr());
        gl::Uniform2f(uniforms.window_size, size.width as f32, size.height as f32);
        gl::Uniform2f(uniforms.glyph_size, size.width as f32 / scale as f32, size.height as f32 / scale as f32);
    }

    (pipeline, gl_state)
}

#[inline(always)]
fn render_frame(gl_context: &WindowedContext,
                engine_context: &EngineContext,
                pipeline: &ShaderPipeline,
                gl_state: &GlState) {

    unsafe {
        gl::ClearColor(0.0, 0.0, 0.0, 1.0);
        gl::Clear(gl::COLOR_BUFFER_BIT);
        gl::DrawArraysInstanced(
            gl::TRIANGLES, 0,
            QUAD_VERTEX_TEX_COORDS_COUNT as _,
        pipeline.glyph_buffer_size as i32);
    }
    gl_context.swap_buffers().unwrap();
}