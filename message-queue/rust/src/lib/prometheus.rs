use std::error;

use prometheus::{IntGauge, Registry, labels};

use crate::Instance;
use crate::runtime::Runtime;

pub struct Prometheus {
    registry: Registry,
    pub test_metric: IntGauge
}

impl Prometheus {
    pub fn new(rt: &Runtime) -> Result<Self,Box<dyn error::Error>> {
        let r = Registry::new_custom(None, Some(rt.to_map()));
        if let Err(e) = r {
            return Err(Box::new(e));
        }

        let s = Self{
            registry: r.unwrap(),
            test_metric: IntGauge::new("test", "Test metric.").unwrap(),
        };
        s.registry.register(Box::new(s.test_metric.clone()))?;
        return Ok(s);
    }

    pub fn push(&self) -> Result<(),Box<dyn error::Error>> {
        let metrics = self.registry.gather();
        let r = prometheus::push_add_metrics(
            "mq-producer",
            labels! {},
            "localhost:9091",
            metrics,
            None,
        );
        if let Err(e) = r {
            return Err(Box::new(e));
        }

        return Ok(());
    }
}

pub fn test_cmd(app: &Instance) -> Result<(), Box<dyn error::Error>> {
    println!("Testing prometheus");
    println!("{:?}", app.runtime.to_map());

    app.prometheus.test_metric.set(2);
    app.prometheus.push()?;

    return Ok(());
}
