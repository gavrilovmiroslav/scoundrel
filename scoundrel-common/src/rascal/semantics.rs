use std::collections::HashMap;

use crate::colors::Color;
use crate::engine::force_quit;
use crate::glyphs::{paint_all_tiles, paint_tile, print_string_colors};
use crate::rascal::parser::{ComponentCallSite, ComponentModifier, RascalExpression};
use crate::rascal::vm::{rascal_value_as_string, RascalValue, RascalVM};
use crate::rascal::world::World;

#[derive(Debug)]
pub enum Position {
    All,
    At((u32, u32)),
}

#[derive(Debug)]
pub enum SemanticChange {
    ValueAssign(String, RascalValue),
    ValueInc(String, RascalValue),
    ValueDec(String, RascalValue),
    SpawnEntity(String, Vec<ComponentCallSite>),
    DestroyEntity(String),
    UpdateComponents(String, Vec<ComponentCallSite>),
    TriggerEvent(ComponentCallSite),
    ConsumeEvent,
    Paint(Position, Option<Color>, Option<Color>),
    Print((u32, u32), (u8, u8, u8), (u8, u8, u8), RascalExpression),
    DebugPrint(RascalExpression),
    Quit,
}

impl RascalVM {
    pub(crate) fn commit_changes(&mut self, world: &mut World, index: usize, values: &mut HashMap<String, RascalValue>, changes: Vec<SemanticChange>) {
        for change in changes {
            match change {
                SemanticChange::ValueInc(name, mod_value) => {
                    if let RascalValue::Num(mod_num) = mod_value {
                        match values.get(&name).unwrap() {
                            RascalValue::Num(num) => {
                                let new_value = RascalValue::Num(*num + mod_num);
                                values.insert(name.clone(), new_value.clone());
                                self.assign_value(world, index, name, new_value);
                            }

                            _ => unreachable!()
                        }
                    }
                }

                SemanticChange::ValueDec(name, mod_value) => {
                    if let RascalValue::Num(num) = values.get(&name).unwrap() {
                        if let RascalValue::Num(mod_num) = mod_value {
                            let new_value = RascalValue::Num(*num - mod_num);
                            values.insert(name.clone(), new_value.clone());
                            self.assign_value(world, index, name, new_value);
                        }
                    }
                }

                SemanticChange::ValueAssign(name, new_value) => {
                    values.insert(name.clone(), new_value.clone());
                    self.assign_value(world, index, name, new_value);
                }

                SemanticChange::SpawnEntity(name, comps) => {
                    let entity = world.create_entity();
                    for comp in comps {
                        self.add_component(world, entity, comp, values);
                    }
                    values.insert(name, RascalValue::Entity(entity));
                }

                SemanticChange::DestroyEntity(entity) => {
                    if let RascalValue::Entity(index) = values.get(&entity).unwrap() {
                        let index = *index as usize;
                        world.entity_bitmap.set(index, false);

                        for comp in &world.component_names {
                            let mut bitmap = world.storage_bitmaps.get_mut(comp).unwrap();
                            bitmap.set(index, false);
                        }
                    }
                }

                SemanticChange::UpdateComponents(name, comps) => {
                    if let Some(RascalValue::Entity(index)) = values.get(&name) {
                        let index = *index;
                        if world.entity_bitmap.get(index) {
                            for comp in comps {
                                match comp.modifier {
                                    ComponentModifier::Default =>
                                        { self.add_component(world, index, comp, values); }
                                    ComponentModifier::Not =>
                                        { world.storage_bitmaps.get_mut(&comp.name).unwrap().set(index, false); }
                                    ComponentModifier::Toggle => {
                                        if world.storage_bitmaps.get(&comp.name).unwrap().get(index) {
                                            world.storage_bitmaps.get_mut(&comp.name).unwrap().set(index, false);
                                        } else {
                                            self.add_component(world, index, comp, values);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                SemanticChange::TriggerEvent(event) => {
                    self.trigger_event(world, event, values);
                }

                SemanticChange::ConsumeEvent => {
                    self.event_consumed = true;
                }

                SemanticChange::Paint(pos, fg, bg) => {
                    match pos {
                        Position::All => {
                            paint_all_tiles(fg, bg);
                        }

                        Position::At(xy) => {
                            paint_tile(xy, fg, bg, self.current_system.as_ref().unwrap().real_priority);
                        }
                    }
                    self.something_printed = true;
                }

                SemanticChange::Print(xy, fg, bg, expr) => {
                    let v = self.interpret_expression(&expr, &values, world).unwrap();
                    let text = rascal_value_as_string(v);

                    print_string_colors((xy.0, xy.1), text.as_str(), Color::from(fg), Color::from(bg),
                                        self.current_system.as_ref().unwrap().real_priority);

                    self.something_printed = true;
                }

                SemanticChange::DebugPrint(expr) => {
                    let v = self.interpret_expression(&expr, &values, world).unwrap();
                    println!("{}", rascal_value_as_string(v));
                }

                SemanticChange::Quit => {
                    force_quit();
                }
            }
        }
    }
}