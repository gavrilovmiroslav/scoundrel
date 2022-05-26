# Scoundrel
> A roguelike engine with devious intent!

[![crates.io](https://img.shields.io/crates/v/scoundrel.svg)](https://crates.io/crates/scoundrel)
[![docs](https://docs.rs/scoundrel/badge.svg)](http://docs.rs/scoundrel/)

# Features

- [x] Easy ASCII rendering (`print_char`, `print_glyph`, `print_string` functions)
- [x] Easy shape stamping and rasterization (rectangles, circles, boolean operations, etc.)
- [x] Easy dungeon generation with underlying graph operations
- [x] Data-driven input contexts, and raw input checking
- [x] Easy to use Entity-Component-System storage with error-handling
- [x] Gameplay functionalities (Field-of-view, pathfinding, etc.)
- [x] Rendering only when needed (you control whether a `redraw` should happen)
- [ ] Sophisticated player flow control
- [ ] Documentation needs to be heavily updated
- [ ] Tutorials need to be rewritten to be up-to-date with version >0.8.0

# Simple example

## Main loop

```rust
pub fn create_hero() -> Entity {
    // our entities are described in terms of their components
    World::spawn((
        Positioned::at(10, 10), // these are some of the common components
        Visible(Glyph::HERO),   // that all rendered things will require
        Sighted(16),            // the next ones are needed only for things with sight
        FieldOfView::empty(),
        PlayerTag))             // this marks that this is a player entity
}

pub fn main() {
    let mut engine = EngineInstance::default();

    create_hero();
    make_dungeon();
    
    while !should_quit() {
        poll_inputs(&mut engine);
        
        if should_redraw() {
            clear_screen();
            engine.render();
        }
    }
}
```

## Dungeon Generation

```rust

pub fn make_dungeon() -> Entity {
    let mut level = MapLevel::default();

    // stamp large rooms
    stamp_rooms(RoomStampDistribution{ 
        width: 8..13, height: 8..13, max_count: 2, min_distance: 2 
    }, &mut level);

    // stamp smaller rooms
    stamp_rooms(RoomStampDistribution{ 
        width: 5..8, height: 5..8, max_count: 20, min_distance: 1 
    }, &mut level);

    // stamp filler rooms
    stamp_rooms(RoomStampDistribution{ 
        width: 3..5, height: 3..5, max_count: 300, min_distance: 1 
    }, &mut level);

    // trunk connects all the rooms with no cycles
    // shrub contains all the remaining non-intersecting edges between rooms
    let (trunk, shrub) = construct_mesh(level.centers.as_slice());

    // add all the corridors for the trunk, making sure to connect all the rooms
    stamp_corridors(CorridorStampDistribution {
        glyph: Glyph::FLOOR,
        probability: Percentage::new(100),
        filter_lengths: None,
        corridor_style: &corridors::straight_edge,
        ignore_already_connected: true,
    }, &trunk, &mut level);

    // add 50% of the rest of the corridors, to make the dungeon more fun
    stamp_corridors(CorridorStampDistribution {
        glyph: Glyph::FLOOR,
        probability: Percentage::new(50),
        filter_lengths: Some(1..8),
        corridor_style: &corridors::straight_edge,
        ignore_already_connected: true,
    }, &shrub, &mut level);

    // fetch the player entity
    let (player_entity, _) = <(PlayerTag,)>::get().unwrap();

    // select a walkable point in the level
    let at = {
        let points = level.walkable.points();
        points[rand::usize(0..points.len())]
    };

    // position the player entity at that point
    player_entity.update((Positioned(at),));

    World::spawn((CurrentLevelTag, level))
}
```

## Gameplay Systems

```rust
pub fn update_fov(player_entity: Entity, level_entity: Entity, 
                  mut level: MapLevel, from: &Point, range: u16) {
    
    assert!(player_entity.has_component::<(FieldOfView,)>());
    assert!(level_entity.has_component::<(MapLevel,)>());

    let fov = level.fov(from, range);
    level_entity.update((level,));
    player_entity.update((fov,));
}

pub fn check_player_movement(dir: &Point, flow: &mut Flow) {
    if !dir.is_zero() {
        // grab these components from the first entity that has them all
        let (player_entity, (Positioned(at), Sighted(range), _)) = 
            <(Positioned, Sighted, PlayerTag)>::get().unwrap();

        let (level_entity, (level, _)) = 
            <(MapLevel, CurrentLevelTag)>::get().unwrap();

        let next_pos = *dir + at;
        
        // the player can walk to this location
        if level.walkable.get(&next_pos) {
            player_entity.update((Positioned(next_pos),));
            
            // update the players field of view
            update_fov(player_entity, level_entity, level, &at, range);
            
            // we only redraw the screen once the player makes an action
            force_redraw();
        }
    }
}
```