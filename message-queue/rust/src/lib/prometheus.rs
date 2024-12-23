use std::error;

use prometheus::{IntGauge, Registry, Gauge};

use crate::Instance;
use crate::runtime::Runtime;
use crate::SampleDesc;

pub struct Prometheus {
    registry: Registry,
    pub test_metric: IntGauge,
    pub messages_total: IntGauge,
    pub messages_per_second: Gauge,
    pub duration_seconds: Gauge,
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
            messages_total: IntGauge::new(
                "messages_total",
                "Messages sent."
            ).unwrap(),
            messages_per_second: Gauge::new(
                "messages_per_second",
                "Messages per second sent."
            ).unwrap(),
            duration_seconds: Gauge::new(
                "duration_seconds",
                "Work duration in seconds."
            ).unwrap(),
        };
        s.registry.register(Box::new(s.test_metric.clone()))?;
        s.registry.register(Box::new(s.messages_total.clone()))?;
        s.registry.register(Box::new(s.messages_per_second.clone()))?;
        s.registry.register(Box::new(s.duration_seconds.clone()))?;
        return Ok(s);
    }

    pub fn push(&self, sdesc: &SampleDesc, worker_id: u64) -> Result<(),Box<dyn error::Error>> {
        let metrics = self.registry.gather();
        let mut labels = sdesc.to_map();
        labels.insert("worker_id".to_string(), worker_id.to_string());
        let r = prometheus::push_add_metrics(
            "mq-producer",
            labels,
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

    let sdesc = SampleDesc{
        n_workers: 4,
        algorithm: "threading",
        mq_system: "postgres",
    };

    app.prometheus.test_metric.set(2);
    app.prometheus.push(&sdesc, 1)?;

    return Ok(());
}
