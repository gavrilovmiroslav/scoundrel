use crate::core::engine::start_engine;
use crate::core::engine::{EngineOptions, ENGINE_STATE};
use crate::core::input::{Input, InputState};
use crate::engine::{EngineInstance};
use crate::graphics::common::gl_error_check;
use crate::graphics::gamepad::gilrs_to_button;
use crate::graphics::glyph_renderer::GlyphRenderer;
use crate::graphics::types::Renderable;
use gilrs::{EventType, Gilrs};
use gl::types::GLboolean;
use glutin::dpi::LogicalSize;
use glutin::*;
use std::ffi::c_void;

impl Default for EngineInstance {
    fn default() -> Self {
        let engine = EngineInstance::new(EngineOptions::default());
        ENGINE_STATE.lock().unwrap().render_state.should_redraw = true;
        engine
    }
}

impl EngineInstance {
    fn new(opts: EngineOptions) -> EngineInstance {
        start_engine(opts.clone());

        let event_loop = EventsLoop::new();

        let window_builder = WindowBuilder::new()
            .with_title(opts.title.as_str())
            .with_resizable(false)
            .with_dimensions(LogicalSize::new(
                opts.window_size.0 as f64,
                opts.window_size.1 as f64,
            ));

        let gl_context = glutin::ContextBuilder::new()
            .with_gl(GlRequest::Specific(Api::OpenGl, (4, 4)))
            .with_vsync(false)
            .build_windowed(window_builder, &event_loop)
            .unwrap();

        let (window_size, pipeline) = render_prepare(&gl_context);
        let gamepad = Gilrs::new().unwrap();

        ENGINE_STATE.lock().unwrap().render_state.screen_size = window_size;

        EngineInstance {
            event_loop,
            gl_context,
            pipeline,
            gamepad,
        }
    }

    pub(crate) fn poll_inputs(&mut self) {
        let engine_state = &mut ENGINE_STATE.lock().unwrap();

        for (_, state) in engine_state.input_state.input_events.iter_mut() {
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
                engine_state.runtime_state.should_quit = true;
                println!("|= Quitting!");
            }

            Event::WindowEvent {
                event: WindowEvent::KeyboardInput { input, .. },
                ..
            } => {
                if let Some(key) = input.virtual_keycode {
                    match input.state {
                        ElementState::Pressed => {
                            engine_state.input_state.input_events.insert(
                                Input::Keyboard(unsafe { std::mem::transmute(key) }),
                                InputState::Pressed,
                            );
                        }

                        ElementState::Released => {
                            engine_state.input_state.input_events.insert(
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
                    engine_state
                        .input_state
                        .input_events
                        .insert(Input::Mouse(button), InputState::Pressed);
                }
                ElementState::Released => {
                    engine_state
                        .input_state
                        .input_events
                        .insert(Input::Mouse(button), InputState::Released);
                }
            },

            Event::WindowEvent {
                event: WindowEvent::CursorMoved { position, .. },
                ..
            } => {
                let mut xy = engine_state.input_state.mouse_position;
                xy.x = position.x as i16 / 16 as i16;
                xy.y = position.y as i16 / 16 as i16;
            }

            _ => {}
        });

        while let Some(gilrs::Event { event, .. }) = self.gamepad.next_event() {
            match event {
                EventType::ButtonPressed(button, _) => {
                    engine_state
                        .input_state
                        .input_events
                        .insert(Input::Gamepad(gilrs_to_button(button)), InputState::Pressed);
                }

                EventType::ButtonReleased(button, _) => {
                    engine_state.input_state.input_events.insert(
                        Input::Gamepad(gilrs_to_button(button)),
                        InputState::Released,
                    );
                }

                _ => {}
            };
        }
    }
}

impl Renderable for EngineInstance {
    fn render(&self) {
        render_frame(&self.gl_context, &self.pipeline);
    }
}

#[allow(dead_code)]
pub fn render<R: Renderable>(renderable: &R) {
    renderable.render();
}

#[inline(always)]
fn render_prepare(gl_context: &WindowedContext) -> ((u32, u32), GlyphRenderer) {
    unsafe { gl_context.make_current().unwrap() };

    let presentation = {
        let engine_state = ENGINE_STATE.lock().unwrap();

        let presentation = engine_state.runtime_state.options.presentation.clone();
        println!("Preparing to use presentation: {}", presentation);
        presentation
    };

    let render_options = {
        let presentations = &ENGINE_STATE.lock().unwrap().runtime_state.presentations;
        presentations.get(presentation.as_str()).unwrap().clone()
    };

    gl::load_with(|symbol| gl_context.get_proc_address(symbol) as *const c_void);
    gl_error_check();

    let size = gl_context.window().get_inner_size().unwrap();
    let (window_size, glyph_renderer) = GlyphRenderer::new(size, &render_options);

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

    (window_size, glyph_renderer)
}

#[inline(always)]
fn render_frame(gl_context: &WindowedContext, pipeline: &GlyphRenderer) {
    let mut engine_state = ENGINE_STATE.lock().unwrap();

    if engine_state.render_state.should_redraw {
        let screen = &mut engine_state.render_state.screen;
        if screen.is_ready() {
            unsafe {
                gl::ClearColor(0.0, 0.0, 0.0, 1.0);
                gl::Clear(gl::COLOR_BUFFER_BIT);
            }

            pipeline.render(screen.glyphs());

            gl_context.swap_buffers().unwrap();
        }

        engine_state.render_state.should_redraw = false;
    }

    engine_state.render_state.screen.reinit_depth();
}
