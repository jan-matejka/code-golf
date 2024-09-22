use std::time::Duration;
use std::collections::HashMap;

pub struct SampleDesc {
    pub n_workers: u64,
    pub algorithm: String,
    pub mq_system: String,
}

impl SampleDesc {
    pub fn to_map(&self) -> HashMap<String,String> {
        let mut m = HashMap::new();
        m.insert("n_workers".to_string(), self.n_workers.to_string());
        m.insert("algorithm".to_string(), self.algorithm.to_string());
        m.insert("mq_system".to_string(), self.mq_system.to_string());
        return m;
    }
}

pub struct WorkerResult {
    pub worker_id: u64,
    pub messages_total: u64,
    pub duration: Duration,
    pub messages_per_second: f64,
}

impl WorkerResult {
    pub fn new(worker_id: u64, messages_total: u64, duration: Duration) -> Self {
        return Self{
            worker_id: worker_id,
            messages_total: messages_total,
            duration: duration,
            messages_per_second:
                messages_total as f64 / duration.as_secs() as f64,
        }
    }
}

pub struct Results {
    pub workers: Vec<WorkerResult>,
    pub messages_total: u64,
    pub duration: Duration,
    pub messages_per_second: f64,
}

impl Results {
    pub fn new(workers: Vec<WorkerResult>) -> Self {
        let mut messages = 0;
        let mut duration = Duration::new(0, 0);
        for w in &workers {
            messages += w.messages_total;
            duration += w.duration;
        }
        return Self{
            workers: workers,
            messages_total: messages,
            duration: duration,
            messages_per_second:
                messages as f64 / duration.as_secs() as f64,
        };
    }
}
