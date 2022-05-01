use gilrs::{EventType, Gilrs};
use std::borrow::BorrowMut;
use std::ffi::c_void;

use gl::types::GLboolean;
use glutin::dpi::LogicalSize;
use glutin::*;

use crate::core::engine;
use crate::core::engine::{reset_should_redraw, should_redraw, start_engine};
use crate::core::engine_options::EngineOptions;
use crate::core::input::{Input, InputState};
use crate::windowing::common::gl_error_check;
use crate::windowing::gamepad::gilrs_to_button;
use crate::windowing::glyph_renderer::GlyphRenderer;

#[no_mangle]
pub static NvOptimusEnablement: u64 = 0x00000001;

#[no_mangle]
pub static AmdPowerXpressRequestHighPerformance: u64 = 0x00000001;

pub struct Scoundrel {
    pub event_loop: EventsLoop,
    pub gl_context: WindowedContext,
    pub pipeline: GlyphRenderer,
    pub gamepad: Gilrs,
    pub is_done: bool,
}

impl Default for Scoundrel {
    fn default() -> Self {
        Scoundrel::new(EngineOptions::default())
    }
}

impl Scoundrel {
    pub fn new(opts: EngineOptions) -> Scoundrel {
        start_engine(opts);

        let mut event_loop = EventsLoop::new();

        let engine_options = engine::ENGINE_OPTIONS.lock().unwrap().clone();

        let window_builder = WindowBuilder::new()
            .with_title(engine_options.title.as_str())
            .with_resizable(false)
            .with_dimensions(LogicalSize::new(
                engine_options.window_size.0 as f64,
                engine_options.window_size.1 as f64,
            ));

        let gl_context = glutin::ContextBuilder::new()
            .with_gl(GlRequest::Specific(Api::OpenGl, (4, 4)))
            .with_vsync(false)
            .build_windowed(window_builder, &event_loop)
            .unwrap();

        let pipeline = render_prepare(&gl_context);
        let gil = Gilrs::new().unwrap();
        let done = false;

        Scoundrel {
            event_loop,
            gl_context,
            pipeline,
            gamepad: gil,
            is_done: done,
        }
    }

    pub fn update_inputs(&mut self) {
        for (_, state) in engine::INPUT_EVENTS.lock().unwrap().iter_mut() {
            match state {
                InputState::Released => {
                    *state = InputState::None;
                }
                _ => {}
            }
        }

        self.event_loop.poll_events(|event| match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                engine::force_quit();

                self.is_done = true;
                println!("|= Quitting!");
            }

            Event::WindowEvent {
                event: WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(key) = input.virtual_keycode {
                    match input.state {
                        ElementState::Pressed => {
                            engine::INPUT_EVENTS.lock().unwrap().insert(
                                Input::Keyboard(unsafe { std::mem::transmute(key) }),
                                InputState::Pressed,
                            );
                        }

                        ElementState::Released => {
                            engine::INPUT_EVENTS.lock().unwrap().insert(
                                Input::Keyboard(unsafe { std::mem::transmute(key) }),
                                InputState::Released,
                            );
                        }
                    };
                }
            }

            Event::WindowEvent {
                event: WindowEvent::MouseInput { button, state, .. },
                ..
            } => match state {
                ElementState::Pressed => {
                    engine::INPUT_EVENTS
                        .lock()
                        .unwrap()
                        .insert(Input::Mouse(button), InputState::Pressed);
                }
                ElementState::Released => {
                    engine::INPUT_EVENTS
                        .lock()
                        .unwrap()
                        .insert(Input::Mouse(button), InputState::Released);
                }
            },

            Event::WindowEvent {
                event: WindowEvent::CursorMoved { position, .. },
                ..
            } => {
                let mut xy = engine::MOUSE_POSITIONS.lock().unwrap();
                xy.borrow_mut().x = position.x as i16 / 16 as i16;
                xy.borrow_mut().y = position.y as i16 / 16 as i16;
            }

            _ => {}
        });

        while let Some(gilrs::Event { id, event, .. }) = self.gamepad.next_event() {
            match event {
                EventType::ButtonPressed(button, _) => {
                    engine::INPUT_EVENTS
                        .lock()
                        .unwrap()
                        .insert(Input::Gamepad(gilrs_to_button(button)), InputState::Pressed);
                }

                EventType::ButtonReleased(button, _) => {
                    engine::INPUT_EVENTS.lock().unwrap().insert(
                        Input::Gamepad(gilrs_to_button(button)),
                        InputState::Released,
                    );
                }

                _ => {}
            };
        }
    }

    pub fn render(&self) {
        render_frame(&self.gl_context, &self.pipeline);
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
        gl::UniformMatrix4fv(
            uniforms.projection,
            1,
            false as GLboolean,
            nalgebra_glm::value_ptr(&glyph_renderer.projection).as_ptr(),
        );
        gl_error_check();
        gl::UniformMatrix4fv(
            uniforms.viewport,
            1,
            false as GLboolean,
            nalgebra_glm::value_ptr(&glyph_renderer.viewport).as_ptr(),
        );
        gl_error_check();
        gl::UniformMatrix4fv(
            uniforms.camera,
            1,
            false as GLboolean,
            nalgebra_glm::value_ptr(&glyph_renderer.camera).as_ptr(),
        );
        gl_error_check();

        gl::Uniform2f(
            uniforms.input_font_bitmap_size,
            render_options.input_font_bitmap_size.0 as f32,
            render_options.input_font_bitmap_size.1 as f32,
        );
        gl_error_check();
        gl::Uniform2f(
            uniforms.input_font_glyph_size,
            render_options.input_font_glyph_size.0 as f32,
            render_options.input_font_glyph_size.1 as f32,
        );
        gl_error_check();
        gl::Uniform2f(
            uniforms.output_glyph_scale,
            render_options.output_glyph_scale.0 as f32,
            render_options.output_glyph_scale.1 as f32,
        );
        gl_error_check();
        gl::Uniform2f(uniforms.window_size, size.width as f32, size.height as f32);
        gl_error_check();
    }

    glyph_renderer
}

#[inline(always)]
fn render_frame(gl_context: &WindowedContext, pipeline: &GlyphRenderer) {
    {
        let screen = engine::SCREEN.read().unwrap();
        if screen.is_ready() && should_redraw() {
            unsafe {
                gl::ClearColor(0.0, 0.0, 0.0, 1.0);
                gl::Clear(gl::COLOR_BUFFER_BIT);
            }

            pipeline.render(screen.glyphs());

            gl_context.swap_buffers().unwrap();
            reset_should_redraw();
        }
    }

    engine::SCREEN.write().unwrap().reinit_depth();
    engine::FRAME_COUNTER.lock().unwrap().tick();
}
