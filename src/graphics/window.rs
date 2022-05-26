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
use glfw::{Action, Context, Window, WindowEvent};

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

        let glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();
        let (mut window, events) =
            glfw.create_window(opts.window_size.0, opts.window_size.1,
                               opts.title.as_str(), glfw::WindowMode::Windowed)
                .expect("Failed to create GLFW window.");

        window.make_current();

        gl_loader::init_gl();
        gl::load_with(|symbol| gl_loader::get_proc_address(symbol) as *const _);

        window.set_all_polling(true);

        let (window_size, pipeline) = render_prepare(&window);
        let gamepad = Gilrs::new().unwrap();

        ENGINE_STATE.lock().unwrap().render_state.screen_size = window_size;

        EngineInstance {
            glfw,
            events,
            window,
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

        self.glfw.poll_events();
        for (_, event) in glfw::flush_messages(&self.events) {
            match event {
                WindowEvent::Close => {
                    engine_state.runtime_state.should_quit = true;
                    println!("|= Quitting!");
                }

                WindowEvent::Key(key, _scancode, action, _modifiers) => {
                    match action {
                        Action::Press => {
                            engine_state.input_state.input_events.insert(
                                Input::Keyboard(unsafe { std::mem::transmute(key) }),
                                InputState::Pressed,
                            );
                        }
                        Action::Release => {
                            engine_state.input_state.input_events.insert(
                                Input::Keyboard(unsafe { std::mem::transmute(key) }),
                                InputState::Released,
                            );
                        }
                        Action::Repeat => {}
                    }
                }

                WindowEvent::MouseButton(button, action, _modifiers) => {
                    match action {
                        Action::Press => {
                            engine_state
                                .input_state
                                .input_events
                                .insert(Input::Mouse(button.into()), InputState::Pressed);
                        }
                        Action::Release => {
                            engine_state
                                .input_state
                                .input_events
                                .insert(Input::Mouse(button.into()), InputState::Released);
                        }
                        Action::Repeat => {}
                    }
                }

                WindowEvent::CursorPos(x, y) => {
                    let mut xy = engine_state.input_state.mouse_position;
                    xy.x = x as i16 / 16 as i16; // TODO
                    xy.y = y as i16 / 16 as i16; // TODO
                }

                _ => {}
            }
        }

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

    pub fn render(&mut self) {
        render_frame(&self.pipeline, &mut self.window);
    }
}

#[allow(dead_code)]
pub fn render<R: Renderable>(renderable: &R) {
    renderable.render();
}

#[inline(always)]
fn render_prepare(window: &Window) -> ((u32, u32), GlyphRenderer) {
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

    let size = (window.get_size().0 as u32, window.get_size().1 as u32);
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
        gl::Uniform2f(uniforms.window_size, size.0 as f32, size.1 as f32);
        gl_error_check();
    }

    (window_size, glyph_renderer)
}

#[inline(always)]
fn render_frame(pipeline: &GlyphRenderer, window: &mut Window) {
    let mut engine_state = ENGINE_STATE.lock().unwrap();

    if engine_state.render_state.should_redraw {
        let screen = &mut engine_state.render_state.screen;
        if screen.is_ready() {
            unsafe {
                gl::ClearColor(0.0, 0.0, 0.0, 1.0);
                gl::Clear(gl::COLOR_BUFFER_BIT);
            }

            pipeline.render(screen.glyphs());
            window.swap_buffers();
        }

        engine_state.render_state.should_redraw = false;
    }

    engine_state.render_state.screen.reinit_depth();
}
