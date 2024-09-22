use std::process::exit;
use std::sync::Arc;
use std::sync::Barrier;
use std::sync::mpsc::channel;
use std::thread;
use std::error;
use std::time::Duration;
use std::time::Instant;

use jmcgmqp::{Instance,test_cmd,WorkerResult,Results,worker};

fn sample_workers(n: u64) -> Result<Results,worker::Error> {
    let mut workers = Vec::new();
    let mut quit_sig_senders = Vec::new();

    let barrier = Arc::new(Barrier::new(n as usize + 1));

    for _ in 0..n {
        let (tx, rx) = channel();
        quit_sig_senders.push(tx);
        let h = worker::new(rx, Arc::clone(&barrier));
        workers.push(h);
    }

    let b = Arc::clone(&barrier);
    b.wait();
    let now = Instant::now();

    thread::sleep(Duration::from_secs(3));

    for s in quit_sig_senders {
        let _ = s.send(true);
        // if we get an error, it means client disconnected; ignore.
    }

    let elapsed = now.elapsed();

    let mut wresults: Vec<WorkerResult> = Vec::new();
    for v in workers {
        let (is_error, n) = v.join().unwrap();
        if is_error {
            return Err(worker::Error::WorkerFailed);
        }
        println!("{}", n);
        wresults.push(WorkerResult::new(0, n, elapsed));
    }
    let results = Results::new(wresults);
    return Ok(results);
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let app = Instance::new()?;
    if app.config.test_prometheus == 1 {
        return test_cmd(&app);
    }
    let mut prev:Option<Results> = None;
    let mut i = 0;
    let base = 2 as u64;
    for i in 1.. {
        let pow = base.checked_pow(i);
        if pow.is_none() {
            eprintln!("Ran out of u64 powers");
            exit(1);
        }
        let rs = sample_workers(pow.unwrap())?;
        println!(
            "Total: {}\nips: {}\n", rs.messages_total, rs.messages_per_second
        );
        if prev.is_some()
        && prev.as_ref().unwrap().messages_per_second >= rs.messages_per_second
        {
            break;
        } else {
            prev = Some(rs);
        }
    }

    let max = base.pow(i) as u32;
    i -= 1;

    for i in i..max {
        let rs = sample_workers(i as u64)?;
        println!(
            "Total: {}\nips: {}\n", rs.messages_total, rs.messages_per_second
        );
        if prev.is_some()
        && prev.as_ref().unwrap().messages_per_second >= rs.messages_per_second
        {
            break;
        } else {
            prev = Some(rs);
        }
    }
    return Ok(())
}
