use std::collections::HashMap;

pub struct SampleDesc {
    pub n_workers: i64,
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
