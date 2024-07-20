use postgres as pg;
use std::sync::mpsc;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::channel;
use std::thread;
use std::error;
use std::time::Duration;
use std::fmt;

#[derive(Debug)]
enum Error {
    WorkerRxDisconnect
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn insert(i: u64) -> Result<(),Box<dyn error::Error>> {
    let mut client = pg::Client::connect("postgres://mq@localhost/mq", pg::NoTls)?;

    let i_sql = i.to_string();
    client.execute("insert into queue (data) values ($1)", &[&i_sql])?;
    return Ok(())
}

fn worker(rx: Receiver<bool>) -> Result<u64, Box<dyn error::Error>> {
    let i: u64 = 0;
    for i in 1.. {
        let r = rx.try_recv();
        if r.is_err() {
            let e = r.unwrap_err();
            if matches!(e, mpsc::TryRecvError::Disconnected) {
                return Err(Box::new(Error::WorkerRxDisconnect));
            }
        }else{
            return Ok(i);
        }
        insert(i)?;
    }
    return Ok(i);
}

fn worker_thread(rx: Receiver<bool>) -> thread::JoinHandle<u64> {
    // Note: can not return boxed dyn error. Print error to stderr and terminate.
    let h = thread::spawn(move || {
        let r = worker(rx);
        if r.is_err() {
            eprintln!("worker error: {}", r.unwrap_err().to_string());
            return 0;
        }

        return r.unwrap();
    });
    return h
}

fn main() {
    let mut handles = Vec::new();
    let mut senders = Vec::new();

    for _ in 1..10 {
        let (tx, rx) = channel();
        senders.push(tx);
        let h = worker_thread(rx);
        handles.push(h);
    }

    thread::sleep(Duration::from_secs(3));

    for s in senders {
        let r = s.send(true);
        if r.is_err() {
            eprintln!("send error: {}", r.unwrap_err().to_string());
        }
    }

    let mut total = 0;
    for v in handles {
        total += v.join().unwrap();
    }

    println!("Total: {}", total);
}
