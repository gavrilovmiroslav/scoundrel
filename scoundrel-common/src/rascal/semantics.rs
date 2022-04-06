use std::collections::HashMap;

use crate::colors::Color;
use crate::engine;
use crate::engine::force_quit;
use crate::glyphs::{paint_all_tiles, paint_tile, print_field, print_string_colors};
use crate::rascal::interpreter::{get_or_insert_into_string_pool, rascal_value_as_string, RascalValue, RascalVM};
use crate::rascal::parser::{ComponentCallSite, ComponentModifier, RascalExpression};
use crate::rascal::value_stack::{Call, ValueContext};
use crate::rascal::world::World;

#[derive(Debug)]
pub enum Position {
    All,
    At((u32, u32)),
}

#[derive(Debug)]
pub enum SemanticChange {
    LetBinding(String, RascalValue),
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
    pub(crate) fn commit_change(&mut self, world: &mut World, values: &mut ValueContext, change: SemanticChange) {
        let index = self.current_index;
        match change {
            SemanticChange::ValueInc(name, mod_value) => {
                if let RascalValue::Num(mod_num) = mod_value {
                    if values.contains_key(&name) {
                        if let RascalValue::Num(num) = values.get(&name).unwrap().get_value().unwrap() {
                            let new_value = RascalValue::Num(num + mod_num);
                            values.add_value(name.clone(), new_value.clone());
                            self.assign_value(values, world, index, name, Call::ByVal(new_value));
                        }
                    } else {
                        let name_index = get_or_insert_into_string_pool(&name);
                        if world.unique_storage.contains_key(&name_index) {
                            if let RascalValue::Num(num) = world.unique_storage.get(&name_index).unwrap() {
                                let new_value = RascalValue::Num(*num + mod_num);
                                world.unique_storage.insert(name_index, new_value);
                            }
                        }
                    }
                }
            }

            SemanticChange::ValueDec(name, mod_value) => {
                if let RascalValue::Num(mod_num) = mod_value {
                    if values.contains_key(&name) {
                        if let RascalValue::Num(num) = values.get(&name).unwrap().get_value().unwrap() {
                            let new_value = RascalValue::Num(num - mod_num);
                            values.add_value(name.clone(), new_value.clone());
                            self.assign_value(values, world, index, name, Call::ByVal(new_value));
                        }
                    } else {
                        let name_index = get_or_insert_into_string_pool(&name);
                        if world.unique_storage.contains_key(&name_index) {
                            if let RascalValue::Num(num) = world.unique_storage.get(&name_index).unwrap() {
                                let new_value = RascalValue::Num(num - mod_num);
                                world.unique_storage.insert(name_index, new_value);
                            }
                        }
                    }
                }
            }

            SemanticChange::ValueAssign(name, new_value) => {
                let name_index = get_or_insert_into_string_pool(&name);
                if world.unique_storage.contains_key(&name_index) {
                    *world.unique_storage.get_mut(&name_index).unwrap() = new_value;
                } else {
                    if let RascalValue::Set(id) = &new_value {
                        values.add_value(name.clone(), new_value.clone());
                        if world.set_storage.contains_key(&name_index) {
                            world.set_storage.insert(name_index, world.set_storage.get(id).unwrap().clone());
                        }
                    } else {
                        values.add_value(name.clone(), new_value.clone());
                        if self.bindings.contains_key(&name) {
                            self.assign_value(values, world, index, name, Call::ByVal(new_value.clone()));
                        }
                    }
                }
            }

            SemanticChange::LetBinding(name, new_value) => {
                values.add_value(name.clone(), new_value.clone());
            }

            SemanticChange::SpawnEntity(name, comps) => {
                let entity = world.create_entity();
                for comp in comps {
                    self.add_component(world, entity, comp, values);
                }
                values.add_value(name, RascalValue::Entity(entity));
            }

            SemanticChange::DestroyEntity(entity) => {
                if let RascalValue::Entity(index) = values.get(&entity).unwrap().get_value().unwrap() {
                    world.entity_bitmap.set(index, false);

                    for comp in &world.component_names {
                        let mut bitmap = world.storage_bitmaps.get_mut(comp).unwrap();
                        bitmap.set(index, false);
                    }
                }
            }

            SemanticChange::UpdateComponents(name, comps) => {
                if let Some(RascalValue::Entity(index)) = values.get(&name).unwrap().get_value() {
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
                let v = self.interpret_expression(&expr, values, world).unwrap();
                if let RascalValue::Field(fid) = v {
                    print_field(&world.field_storage.get(&fid).unwrap().field_map, Color::from(fg), Color::from(bg),
                                self.current_system.as_ref().unwrap().real_priority);
                } else {
                    let text = rascal_value_as_string(v);

                    print_string_colors((xy.0, xy.1), text.as_str(), Color::from(fg), Color::from(bg),
                                        self.current_system.as_ref().unwrap().real_priority);

                    self.something_printed = true;
                }
            }

            SemanticChange::DebugPrint(expr) => {
                println!("VALUES: {:?}", values);
                let v = self.interpret_expression(&expr, values, world).unwrap();
                println!("{}", rascal_value_as_string(v));
            }

            SemanticChange::Quit => {
                force_quit();
            }
        }
    }
}