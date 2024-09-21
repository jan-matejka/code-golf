use std::error;
use std::collections::HashMap;

use chrono::prelude::*;
use uuid::Uuid;
use platform_info::{PlatformInfo, PlatformInfoAPI, UNameAPI};

use rustc_version_runtime;

use crate::Config;

#[derive(Debug)]
pub struct Runtime {
    pub ctime: DateTime<Local>,
    pub uuid: Uuid,
    pub lang: String,
    pub lang_version: rustc_version_runtime::Version,
    pub runtime: rustc_version_runtime::VersionMeta,
    pub os: String,
    pub kernel: String,
    pub arch: String,
}

impl Runtime {
    pub fn new() -> Result<Self,Box<dyn error::Error>> {
        let r = PlatformInfo::new();
        if let Err(e) = r {
            // work around PlatformInfo::new() returning
            // <dyn Error + Send + sync>
            // which makes it unusable with `?` operator since we return just error.
            // wtf?
            return Err(e);
        }
        let pi = r.unwrap();
        return Ok(Self{
            ctime: Local::now(),
            uuid: Uuid::new_v4(),
            lang: "rust".to_string(),
            lang_version: rustc_version_runtime::version(),
            runtime: rustc_version_runtime::version_meta(),
            os: pi.sysname().to_str().unwrap().to_string(),
            kernel: pi.release().to_str().unwrap().to_string(),
            arch: pi.machine().to_str().unwrap().to_string()
        });
    }
}

pub struct Instance {
    pub config: Config,
    pub runtime: Runtime,
}

impl Instance {
    pub fn new() -> Result<Self,Box<dyn error::Error>> {
        return Ok(
            Self{
                config: Config::new()?,
                runtime: Runtime::new()?,
            }
        );
    }
}
