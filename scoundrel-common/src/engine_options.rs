use structopt::*;

#[derive(Debug, StructOpt, Clone)]
pub struct EngineOptions {
    /// Vertical synchronization - true or false
    #[structopt(short="v", long)]
    pub vsync: bool,
}

impl Default for EngineOptions {
    fn default() -> Self {
        EngineOptions {
            vsync: false,
        }
    }
}

impl EngineOptions {
    pub fn get_from_command_line() -> EngineOptions {
        EngineOptions::from_args()
    }
}