use std::borrow::BorrowMut;
use std::ffi::c_void;
use std::time::Duration;

use gl::types::GLboolean;
use glutin::*;
use glutin::dpi::LogicalSize;

use scoundrel_common::engine;
use scoundrel_common::engine::snoop_for_data_changes;
use scoundrel_common::keycodes::{KeyState, MouseState};

use crate::common::gl_error_check;
use crate::glyph_renderer::GlyphRenderer;

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

    let pipeline = render_prepare(&gl_context);

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

        if let Some(e) = snoop_for_data_changes() {
            println!("{:?}", e);
        }

        render_frame(&gl_context, &pipeline);
    }
}

#[inline(always)]
fn render_prepare(gl_context: &WindowedContext) -> GlyphRenderer {
    unsafe { gl_context.make_current().unwrap() };

    let presentation = engine::ENGINE_OPTIONS.lock().unwrap().presentation.clone();
    println!("Preparing to use presentation: {}", presentation);

    let render_options = {
        let presentations = engine::PRESENTATIONS.lock().unwrap();
        presentations.get(presentation.as_str()).unwrap().clone()
    };

    gl::load_with(|symbol| gl_context.get_proc_address(symbol) as *const c_void);
    gl_error_check();

    let size = gl_context.window().get_inner_size().unwrap();
    let glyph_renderer = GlyphRenderer::new(size, &render_options);

    let uniforms = glyph_renderer.get_uniforms();
    unsafe {
        gl::UniformMatrix4fv(uniforms.projection, 1, false as GLboolean, nalgebra_glm::value_ptr(&glyph_renderer.projection).as_ptr());
        gl_error_check();
        gl::UniformMatrix4fv(uniforms.viewport, 1, false as GLboolean, nalgebra_glm::value_ptr(&glyph_renderer.viewport).as_ptr());
        gl_error_check();
        gl::UniformMatrix4fv(uniforms.camera, 1, false as GLboolean, nalgebra_glm::value_ptr(&glyph_renderer.camera).as_ptr());
        gl_error_check();

        gl::Uniform2f(uniforms.input_font_bitmap_size, render_options.input_font_bitmap_size.0 as f32, render_options.input_font_bitmap_size.1 as f32);
        gl_error_check();
        gl::Uniform2f(uniforms.input_font_glyph_size, render_options.input_font_glyph_size.0 as f32, render_options.input_font_glyph_size.1 as f32);
        gl_error_check();
        gl::Uniform2f(uniforms.output_glyph_scale, render_options.output_glyph_scale.0 as f32, render_options.output_glyph_scale.1 as f32);
        gl_error_check();
        gl::Uniform2f(uniforms.window_size, size.width as f32, size.height as f32);
        gl_error_check();
    }

    glyph_renderer
}

#[inline(always)]
fn render_frame(gl_context: &WindowedContext,
                pipeline: &GlyphRenderer,) {

    if engine::should_redraw() {
        let screen = engine::SCREEN.read().unwrap().clone();
        if screen.is_ready() {
            engine::clean_redraw();

            unsafe {
                gl::ClearColor(0.0, 0.0, 0.0, 1.0);
                gl::Clear(gl::COLOR_BUFFER_BIT);
            }

            pipeline.render(screen.glyphs());

            gl_context.swap_buffers().unwrap();
        }
    } else {
        std::thread::sleep(Duration::from_millis(1));
    }

    engine::FRAME_COUNTER.lock().unwrap().tick();
}