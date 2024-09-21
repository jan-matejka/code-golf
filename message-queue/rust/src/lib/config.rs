use std::env;
use std::error;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("")]
    IntoStringFailed(String)
}

#[derive(Debug)]
pub struct Config {
    pub test_prometheus: i64
}

impl Config {
    fn igetenv(name: &str, default: i64) -> Result<i64,Box<dyn error::Error>> {
        if let Some(x) = env::var_os(name) {
            let r = x.into_string();
            if r.is_err() {
                return Err(
                    Box::new(Error::IntoStringFailed(
                        String::from(name)
                    ))
                );
            }
            return Ok(r.unwrap().parse::<i64>()?);
        } else {
            return Ok(default);
        }
    }

    pub fn new() -> Result<Self,Box<dyn error::Error>> {
        return Ok(Self{
            test_prometheus: Config::igetenv("TEST_PROMETHEUS", 0)?
        });
    }
}
