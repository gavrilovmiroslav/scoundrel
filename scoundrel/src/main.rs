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

        let entity = world.create_entity();
        world.add_component(entity, "HasHealth".to_string(), vec![10u8, 0u8, 0u8, 0u8]);
        world.add_component(entity, "HasDamage".to_string(), vec![10u8, 0u8, 0u8, 0u8]);
        world.add_component(entity, "IsBurning".to_string(), vec![]);

        let entity = world.create_entity();
        world.add_component(entity, "IsDoor".to_string(), vec![0u8, 0u8, 0u8, 0u8]);
        world.add_component(entity, "IsAt".to_string(),  vec![0u8, 0u8, 0u8, 0u8,   0u8, 0u8, 0u8, 0u8]);
        world.add_component(entity, "HasGlyph".to_string(), vec![0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8]);
    }

    engine.logic.push(core_logic::create_input_checker);
    engine.logic.push(core_logic::create_fps_tracker);

    engine.run(windowing::window_event_loop);
}
