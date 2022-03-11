use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
#[derive(Serialize, Deserialize)]
pub struct EngineOptions {
    pub vsync: bool,
    pub window_size: (u32, u32),
    pub title: String,
    pub presentation: String,
}

impl Default for EngineOptions {
    fn default() -> Self {
        ron::from_str(std::fs::read_to_string("resources/data/config.ron").unwrap().as_str()).unwrap()
    }
}