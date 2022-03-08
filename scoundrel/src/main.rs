use scoundrel_common::engine_options::EngineOptions;
use scoundrel_core::core_logic;
use scoundrel_core::engine::Engine;
use scoundrel_windowing::windowing;

fn main() {
    let mut engine = Engine::new(EngineOptions::default());

    for sys in [
        core_logic::clear_screen,
        core_logic::pass_input_events_to_rascal,
        core_logic::track_fps,
        core_logic::exit_game_from_script
    ] { engine.support_systems.push(sys); }

    engine.run(windowing::window_event_loop);
}
