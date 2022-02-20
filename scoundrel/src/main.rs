use scoundrel_common::engine_options::EngineOptions;
use scoundrel_core::core_logic;
use scoundrel_core::engine::Engine;
use scoundrel_windowing::windowing;

fn main() {
    let mut engine = Engine::new(EngineOptions::default());

    engine.logic.push(core_logic::create_input_checker);
    engine.logic.push(core_logic::create_fps_tracker);

    engine.run(windowing::window_event_loop);
}
