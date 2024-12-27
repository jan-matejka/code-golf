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
    pub fn getenv<T: FromStr>(name: &str, default: T) -> Result<T,Box<dyn error::Error>>
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

#[cfg(test)]
pub mod tests {
    use super::*;
    use lazy_static::lazy_static;

    #[derive(Debug)]
    pub struct TestConfig {
        pub pg_test_dsn: String,
        pub pg_test_root_dsn: String,
        pub pg_test_mq_dsn: String,
    }

    impl TestConfig {
        pub fn new() -> Result<Self,Box<dyn error::Error>> {
            let base = Config::getenv::<String>(
                "PG_TEST_DSN", "localhost:5433".to_string()
            )?;
            return Ok(Self{
                pg_test_dsn: base.clone(),
                pg_test_root_dsn: format!("postgres://postgres@{}", &base),
                pg_test_mq_dsn: format!("postgres://mq@{}/test", &base),
            });
        }
    }

    lazy_static! {
        pub static ref TCG: TestConfig = TestConfig::new().unwrap();
    }
}
