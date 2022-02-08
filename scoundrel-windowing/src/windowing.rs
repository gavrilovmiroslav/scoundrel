use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::sync::atomic::Ordering;
use crow::{BlendMode, Context, DrawConfig, DrawTarget, Texture};
use crow::glutin::event::{ElementState, Event, MouseButton, VirtualKeyCode, WindowEvent};
use crow::glutin::event_loop::{ControlFlow, EventLoop};
use crow::glutin::platform::windows::EventLoopExtWindows;
use crow::glutin::window::WindowBuilder;
use rand::Rng;
use scoundrel_common::keycodes::{KeyState, MouseState};
use scoundrel_common::engine_context::EngineContext;

const SKIP_FRAME: u64 = 10;

fn transmute_keycode(vk: VirtualKeyCode) -> KeyState {
    unsafe { std::mem::transmute(vk) }
}

fn transmute_mouse(mb: MouseButton, st: ElementState) -> MouseState {
    MouseState::new(unsafe { std::mem::transmute(mb) }, unsafe { std::mem::transmute(st) })
}

pub fn window_event_loop(shared_data: EngineContext) {
    let mut event_loop = EventLoop::new_any_thread();
    let mut frame = 0u64;
    let mut context = Context::new(WindowBuilder::new(), &event_loop).unwrap();

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

    event_loop.run(move |event: Event<()>, _window_target: _, control_flow: &mut ControlFlow| match event {
        Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => {
            *control_flow = ControlFlow::Exit;
            shared_data.should_quit.store(true, Ordering::SeqCst);

            println!("|= Quitting!");
        },

        Event::MainEventsCleared => {
            if frame % SKIP_FRAME == 0 {
                let mut surface = context.surface();
                context.clear_color(&mut surface, (0.0, 0.0, 0.0, 1.0));

                let mut config = DrawConfig::default();
                config.scale = (2, 2);
                for i in (0..1024).step_by(16) {
                    for j in (0..768).step_by(16) {

                        config.color_modulation = [
                            [rng.gen::<f32>(), 0.0, 0.0, 0.0],
                            [0.0, rng.gen::<f32>(), 0.0, 0.0],
                            [0.0, 0.0, rng.gen::<f32>(), 0.0],
                            [0.0, 0.0, 0.0, 1.0],
                        ];

                        context.draw(&mut surface, &textures_by_char[&(rng.gen_range(1u16..128u16))], (i, j), &config);
                    }
                }
                context.present(surface).unwrap();

            }

            frame += 1;
            shared_data.frame_counter.fetch_add(1, Ordering::SeqCst);
        },

        Event::WindowEvent { event: WindowEvent::KeyboardInput {input, .. }, .. } => {
            if let Some(key) = input.virtual_keycode {
                shared_data.keyboard_events.lock().unwrap().push_back(transmute_keycode(key));
            }
        },

        Event::WindowEvent { event: WindowEvent::MouseInput{ button, state, .. }, .. } => {
            shared_data.mouse_events.lock().unwrap().push_back(transmute_mouse(button, state))
        },

        Event::WindowEvent { event: WindowEvent::CursorMoved { position, ..}, .. } => {
            let mut xy = shared_data.mouse_position.lock().unwrap();
            xy.borrow_mut().x = position.x as i32 / 16;
            xy.borrow_mut().y = position.y as i32 / 16;
        }

        _ => {},
    });
}