use scoundrel_common::engine::rebuild_world;
use scoundrel_common::engine::WORLD;
use scoundrel_common::engine_options::EngineOptions;
use scoundrel_common::rascal::parser::parse_rascal;
use scoundrel_core::core_logic;
use scoundrel_core::engine::Engine;
use scoundrel_windowing::windowing;

fn main() {
    let mut engine = Engine::new(EngineOptions::default());

    let ast = parse_rascal(include_str!("..\\prelude.rasc").trim());
    rebuild_world(ast);

    {
        let mut world = WORLD.lock().unwrap();

        let player_entity = world.create_entity();
        world.add_tag(player_entity, "IsPlayer");
    }

    engine.logic.push(core_logic::pass_input_events_to_rascal);
    engine.logic.push(core_logic::create_fps_tracker);
    engine.logic.push(core_logic::create_exit_game);

    engine.run(windowing::window_event_loop);
}
