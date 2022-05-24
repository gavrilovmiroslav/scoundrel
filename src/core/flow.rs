
pub enum Flow {
    Continue,
    Done,
}

impl Flow {
    pub fn is_done(&self) -> bool {
        match self {
            Flow::Continue => false,
            Flow::Done => true,
        }
    }

    pub fn done(&mut self) {
        *self = Flow::Done;
    }
}
