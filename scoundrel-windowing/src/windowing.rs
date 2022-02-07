use std::sync::atomic::Ordering;
use crow::{Context, DrawConfig, Texture};
use crow::glutin::event::{Event, VirtualKeyCode, WindowEvent};
use crow::glutin::event_loop::{ControlFlow, EventLoop};
use crow::glutin::platform::windows::EventLoopExtWindows;
use crow::glutin::window::WindowBuilder;
use scoundrel_common::keycodes::KeyCode;
use scoundrel_common::engine_context::EngineContext;

fn transmute_keycode(vk: VirtualKeyCode) -> KeyCode {
    unsafe { std::mem::transmute(vk) }
}

pub fn window_event_loop(shared_data: EngineContext) {
    let mut event_loop = EventLoop::new_any_thread();

    let mut context = Context::new(WindowBuilder::new(), &event_loop).unwrap();

    let texture = Texture::load(&mut context, "./textures/8x8glyphs.png").unwrap();

    event_loop.run(move |event: Event<()>, _window_target: _, control_flow: &mut ControlFlow| match event {
        Event::WindowEvent {
            event: WindowEvent::CloseRequested, ..
        } => {
            *control_flow = ControlFlow::Exit;
            shared_data.should_quit.store(true, Ordering::SeqCst);

            println!("|= Quitting!");
        },

        Event::MainEventsCleared => {
            context.window().request_redraw();
            shared_data.frame_counter.fetch_add(1, Ordering::Relaxed);
        },

        Event::RedrawRequested(_) => {
            let mut surface = context.surface();
            context.clear_color(&mut surface, (0.4, 0.4, 0.8, 1.0));

            context.draw(&mut surface, &texture, (0, 0), &DrawConfig::default());
            context.present(surface).unwrap();
        },

        Event::WindowEvent { event: WindowEvent::KeyboardInput {input, .. }, .. } => {
            shared_data.keyboard_events.lock().unwrap().push_back(transmute_keycode(input.virtual_keycode.unwrap()));
        },

        _ => {},
    });
}