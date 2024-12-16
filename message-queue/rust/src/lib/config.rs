use std::str::FromStr;
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
    pub test_prometheus: i64,
    pub duration: u64,
    pub pg_telemetry_dsn: String,
}

impl Config {
    fn getenv<T: FromStr>(name: &str, default: T) -> Result<T,Box<dyn error::Error>>
    where <T as FromStr>::Err: 'static, <T as FromStr>::Err: error::Error
    {
        if let Some(x) = env::var_os(name) {
            let r = x.into_string();
            if r.is_err() {
                return Err(
                    Box::new(Error::IntoStringFailed(
                        String::from(name)
                    ))
                );
            }
            let p = r.unwrap().parse::<T>();
            if let Err(e) = p {
                return Err(Box::new(e));
            }
            return Ok(p.unwrap());
        } else {
            return Ok(default);
        }
    }

    pub fn new(
        pg_telemetry_dsn: Option<String>
    ) -> Result<Self,Box<dyn error::Error>> {
        let dsn = pg_telemetry_dsn.unwrap_or(
            Config::getenv::<String>(
                "PG_TELEMETRY_DSN", "postgres://mq@localhost/mq".to_string()
            )?
        );
        return Ok(Self{
            test_prometheus: Config::getenv::<i64>("TEST_PROMETHEUS", 0)?,
            duration: Config::getenv::<u64>("DURATION", 3)?,
            pg_telemetry_dsn: dsn,
        });
    }
}
