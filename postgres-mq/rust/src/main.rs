use postgres as pg;
use std::process::exit;
use std::sync::Arc;
use std::sync::Barrier;
use std::sync::mpsc;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::channel;
use std::thread;
use std::error;
use std::time::Duration;
use std::time::Instant;
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

fn insert(c: &mut pg::Client, i: u64) -> Result<(),Box<dyn error::Error>> {
    let i_sql = i.to_string();
    c.execute("insert into queue (data) values ($1)", &[&i_sql])?;
    return Ok(())
}

fn worker(rx: Receiver<bool>, pg_barrier: Arc<Barrier>) -> Result<u64, Box<dyn error::Error>> {
    let mut client = pg::Client::connect("postgres://mq@localhost/mq", pg::NoTls)?;
    client.execute("select 1", &[])?;

    pg_barrier.wait();

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
        insert(&mut client, i)?;
    }
    return Ok(i);
}

fn worker_thread(rx: Receiver<bool>, barrier: Arc<Barrier>) -> thread::JoinHandle<u64> {
    // Note: can not return boxed dyn error. Print error to stderr and terminate.
    let h = thread::spawn(move || {
        let r = worker(rx, barrier);
        if r.is_err() {
            eprintln!("worker error: {}", r.unwrap_err().to_string());
            return 0;
        }

        return r.unwrap();
    });
    return h
}

fn sample_workers(n: u64) -> (u64, f64) {
    let now = Instant::now();

    let mut workers = Vec::new();
    let mut quit_sig_senders = Vec::new();

    let barrier = Arc::new(Barrier::new(n as usize));

    for _ in 0..n {
        let (tx, rx) = channel();
        quit_sig_senders.push(tx);
        let h = worker_thread(rx, Arc::clone(&barrier));
        workers.push(h);
    }


    thread::sleep(Duration::from_secs(3));

    for s in quit_sig_senders {
        let _ = s.send(true);
        // if we get an error, it means client disconnected; ignore.
    }

    let mut total = 0;
    for v in workers {
        let n = v.join().unwrap();
        println!("{}", n);
        total += n
    }

    let elapsed = now.elapsed();
    let ips = total as f64 / elapsed.as_secs() as f64;

    return (total, ips);
}

fn main() {
    let mut last = 0 as f64;
    for i in 1.. {
        let base = 2 as u64;
        let pow = base.checked_pow(i);
        if pow.is_none() {
            eprintln!("Ran out of u64 powers");
            exit(1);
        }
        let (total, ips) = sample_workers(pow.unwrap());
        println!("Total: {}\nips: {}\n", total, ips);
        if last >= ips {
            break;
        } else {
            last = ips;
        }
    }
}
