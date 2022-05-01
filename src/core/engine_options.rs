use serde::{Deserialize, Serialize};
use std::fs;
use std::fs::File;
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EngineOptions {
    pub vsync: bool,
    pub window_size: (u32, u32),
    pub title: String,
    pub presentation: String,
}

impl Default for EngineOptions {
    fn default() -> Self {
        match Path::exists(Path::new("resources/data/config.ron")) {
            true => ron::from_str(
                std::fs::read_to_string("resources/data/config.ron")
                    .unwrap()
                    .as_str(),
            )
            .unwrap(),
            false => {
                std::fs::create_dir_all("resources/data/").unwrap();
                std::fs::create_dir_all("resources/data/presentations").unwrap();
                std::fs::create_dir_all("resources/data/textures").unwrap();
                fs::write(
                    "resources/data/config.ron",
                    include_str!("cache/config.ron"),
                )
                .unwrap();

                fs::write(
                    "resources/data/presentations/8x8glyphs.ron",
                    include_str!("cache/8x8glyphs.ron"),
                )
                .unwrap();
                fs::write(
                    "resources/data/textures/8x8glyphs.png",
                    include_bytes!("cache/8x8glyphs.png"),
                )
                .unwrap();
                ron::from_str(
                    std::fs::read_to_string("resources/data/config.ron")
                        .unwrap()
                        .as_str(),
                )
                .unwrap()
            }
        }
    }
}
