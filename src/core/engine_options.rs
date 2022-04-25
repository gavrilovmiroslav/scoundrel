use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EngineOptions {
    pub vsync: bool,
    pub window_size: (u32, u32),
    pub title: String,
    pub presentation: String,
    pub scripts: Vec<String>,
    pub use_rascal: bool,
}

impl Default for EngineOptions {
    fn default() -> Self {
        ron::from_str(
            std::fs::read_to_string("../../../resources/data/config.ron")
                .unwrap()
                .as_str(),
        )
        .unwrap()
    }
}
