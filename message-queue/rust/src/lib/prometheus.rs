use std::error;

use crate::Instance;

pub fn test_cmd(app: &Instance) -> Result<(), Box<dyn error::Error>> {
    println!("Testing prometheus");
    println!("{:?}", app.runtime);
    return Ok(());
}
