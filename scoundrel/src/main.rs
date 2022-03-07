use scoundrel_common::engine::rebuild_world;
use scoundrel_common::engine::WORLD;
use scoundrel_common::engine_options::EngineOptions;
use scoundrel_common::rascal::parser::parse_rascal;
use scoundrel_common::rascal::vm::num;
use scoundrel_common::rascal::world::AddComponent;
use scoundrel_core::core_logic;
use scoundrel_core::engine::Engine;
use scoundrel_windowing::windowing;

fn main() {
    let mut engine = Engine::new(EngineOptions::default());

    let ast = parse_rascal(include_str!("..\\prelude.rasc").trim());
    rebuild_world(ast);

    {
        let mut world = WORLD.lock().unwrap();
        let entity = world.create_entity();
        world.add_tag(entity, "IsPlayer");
        world.add_component(entity, "IsAt", vec![ num(0), num(0) ]);
    }

    engine.support_systems.push(core_logic::pass_input_events_to_rascal);
    engine.support_systems.push(core_logic::track_fps);
    engine.support_systems.push(core_logic::exit_game_from_script);

    engine.run(windowing::window_event_loop);
}
