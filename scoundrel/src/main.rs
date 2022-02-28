use scoundrel_common::engine_options::EngineOptions;
use scoundrel_core::core_logic;
use scoundrel_core::engine::Engine;
use scoundrel_windowing::windowing;

use scoundrel_common::ecs::parser::parse_rascal;
use scoundrel_common::ecs::world::World;

fn main() {
    let mut world = World::default();
    let ast = parse_rascal(include_str!("..\\prelude.rasc").trim());

    for rv in ast {
        if rv.is_component() {
            world.register_component(rv);
        }
    }

    return;

/*    let entity = world.create_entity();
    world.add_component(entity, "HasHealth".to_string(), vec![ 10u8, 0u8, 0u8, 0u8 ]);

    let entity = world.create_entity();
    world.add_component(entity, "HasHealth".to_string(), vec![ 11u8, 0u8, 0u8, 0u8 ]);

    let slice = &world.storage_pointers.get(&"HasHealth".to_string()).unwrap()[0..10];
    for x in slice {
        print!("{} ", x);
    }*/

    let mut engine = Engine::new(EngineOptions::default());

    engine.logic.push(core_logic::create_input_checker);
    engine.logic.push(core_logic::create_fps_tracker);

    engine.run(windowing::window_event_loop);
}
