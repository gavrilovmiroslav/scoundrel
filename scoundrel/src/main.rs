use scoundrel_common::engine::rebuild_world;
use scoundrel_common::engine_options::EngineOptions;
use scoundrel_common::rascal::world::send_start_event;
use scoundrel_core::core_logic;
use scoundrel_core::engine::Engine;
use scoundrel_windowing::windowing;

fn main() {
    let mut engine = Engine::new(EngineOptions::default());

    rebuild_world("default.rascal");
    rebuild_world("sokoban.rascal");

    send_start_event();

    for sys in [
        core_logic::clear_screen,
        core_logic::pass_input_events_to_rascal,
        core_logic::track_fps,
    ] { engine.pre_support_systems.push(sys); }

    for sys in [
        // add systems here
    ] { engine.post_support_systems.push(sys); }

    engine.run(windowing::window_event_loop);
}
