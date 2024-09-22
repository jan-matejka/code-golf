use std::error;
use std::collections::HashMap;

use chrono::prelude::*;
use uuid::Uuid;
use platform_info::{PlatformInfo, PlatformInfoAPI, UNameAPI};

use rustc_version_runtime;

use crate::Config;
use crate::prometheus::Prometheus;

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

    pub fn to_map(&self) -> HashMap<String,String> {
        let rt_string = format!(
            "{}{}",
            self.runtime.short_version_string,
            match self.runtime.llvm_version.as_ref() {
                Some(x) => { format!(" llvm {}", x.to_string()) }
                None => { "".to_string() }
            }
        );

        let mut m = HashMap::new();
        m.insert("ctime".to_string(), self.ctime.to_rfc3339());
        m.insert("uuid".to_string(), self.uuid.to_string());
        m.insert("lang".to_string(), self.lang.to_owned());
        m.insert("lang_version".to_string(), self.lang_version.to_string());
        m.insert("runtime".to_string(), rt_string);
        m.insert("os".to_string(), self.os.to_owned());
        m.insert("kernel".to_string(), self.kernel.to_owned());
        m.insert("arch".to_string(), self.arch.to_owned());
        return m;
    }
}

pub struct Instance {
    pub config: Config,
    pub runtime: Runtime,
    pub prometheus: Prometheus,
}

impl Instance {
    pub fn new() -> Result<Self,Box<dyn error::Error>> {
        let r = Runtime::new()?;
        let p = Prometheus::new(&r)?;
        return Ok(
            Self{
                config: Config::new()?,
                runtime: r,
                prometheus: p,
            }
        );
    }
}
