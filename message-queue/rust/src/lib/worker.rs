use std::time::Duration;
use std::collections::HashMap;
use std::sync::mpsc::Receiver;
use std::sync::{Arc,Barrier,mpsc};
use std::thread;
use std::error;

use thiserror::Error;
use postgres as pg;

#[derive(Debug, Error)]
pub enum Error {
    #[error("WorkerRxDisconnect")]
    WorkerRxDisconnect,
    #[error("WorkerFailed")]
    WorkerFailed
}

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

pub fn new(rx: Receiver<bool>, barrier: Arc<Barrier>) -> thread::JoinHandle<(bool, u64)> {
    // Note: can not return boxed dyn error. Print error to stderr and terminate.
    let h = thread::spawn(move || {
        let r = worker(rx, barrier);
        if r.is_err() {
            eprintln!("worker error: {}", r.unwrap_err().to_string());
            return (true, 0);
        }

        return (false, r.unwrap());
    });
    return h
}

fn make_client() -> Result<pg::Client, pg::Error>  {
    let mut client = pg::Client::connect("postgres://mq@localhost/mq", pg::NoTls)?;
    client.execute("select 1", &[])?;
    return Ok(client);
}

fn worker(rx: Receiver<bool>, pg_barrier: Arc<Barrier>) -> Result<u64, Box<dyn error::Error>> {
    let r = make_client();
    if r.is_err() {
        pg_barrier.wait();
        unsafe {
            // have to use unchecked because Client does not implement Debug trait
            return Err(Box::new(r.unwrap_err_unchecked()));
        }
    }
    let mut client = r.unwrap();
    pg_barrier.wait();

    let i: u64 = 0;
    for i in 1.. {
        if worker_check_quit(&rx).unwrap() {
            return Ok(i);
        }
        insert(&mut client, i)?;
    }
    return Ok(i);
}

fn worker_check_quit(rx: &Receiver<bool>) -> Result<bool,Box<dyn error::Error>> {
    let r = rx.try_recv();
    if r.is_err() {
        let e = r.unwrap_err();
        if matches!(e, mpsc::TryRecvError::Disconnected) {
            return Err(Box::new(Error::WorkerRxDisconnect));
        }
    }else{
        return Ok(true);
    }
    return Ok(false);
}

fn insert(c: &mut pg::Client, i: u64) -> Result<(),Box<dyn error::Error>> {
    let i_sql = i.to_string();
    c.execute("insert into queue (data) values ($1)", &[&i_sql])?;
    return Ok(())
}
