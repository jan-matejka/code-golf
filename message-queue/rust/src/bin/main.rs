use std::process::exit;
use std::sync::Arc;
use std::sync::Barrier;
use std::sync::mpsc::channel;
use std::thread;
use std::error;
use std::time::Duration;

use jmcgmqp::{Instance,test_cmd,WorkerResult,Results,worker,SampleDesc};

fn sample_workers(app: &mut Instance, n: u64) -> Result<Results,Box<dyn error::Error>> {
    println!("Spawning {} workers", n);
    let mut workers = Vec::new();
    let mut quit_sig_senders = Vec::new();

    let barrier = Arc::new(Barrier::new(n as usize + 1));

    for i in 0..n {
        let (tx, rx) = channel();
        quit_sig_senders.push(tx);
        workers.push(worker::new(i, rx, Arc::clone(&barrier)));
    }

    let b = Arc::clone(&barrier);
    b.wait();

    println!("Waiting");
    for i in (1..=app.config.duration).rev() {
        println!("{}", i);
        thread::sleep(Duration::from_secs(1));
    }

    for s in quit_sig_senders {
        let _ = s.send(true);
        // if we get an error, it means client disconnected; ignore.
    }

    let sdesc = SampleDesc{
        n_workers: n,
        algorithm: "threading",
        mq_system: "postgres",
    };

    let mut wresults: Vec<WorkerResult> = Vec::new();
    for v in workers {
        let r = v.join().unwrap();
        if let Err(e) = r {
            return Err(Box::new(e));
        }
        let wr = r.unwrap();
        println!("{}: {}", wr.worker_id, wr.messages_total);

        app.prometheus.messages_total.set(wr.messages_total.clone() as i64);
        app.prometheus.messages_per_second.set(wr.messages_per_second.clone());
        app.prometheus.duration_seconds.set(wr.duration.as_secs_f64().clone());
        let worker_id = wr.worker_id;

        wresults.push(wr);
        app.prometheus.push(&sdesc, worker_id)?;
    }
    let results = Results::new(wresults);
    app.postgres.push(&app.runtime, &sdesc, &results)?;
    return Ok(results);
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut app = Instance::new()?;
    if app.config.test_prometheus == 1 {
        return test_cmd(&app);
    }
    let mut prev:Option<Results> = None;
    let mut exp:u32 = 0;
    let base = 2 as u64;
    loop {
        let pow = base.checked_pow(exp);
        if pow.is_none() {
            eprintln!("Ran out of u64 powers");
            exit(1);
        }
        let rs = sample_workers(&mut app, pow.unwrap())?;
        println!("{}", rs);
        if prev.is_some()
        && prev.as_ref().unwrap().messages_per_second >= rs.messages_per_second
        {
            break;
        } else {
            prev = Some(rs);
        }
        exp += 1;
    }

    let max = base.pow(exp);
    let i = base.pow(exp-1) + 1;
    for i in i..max {
        let rs = sample_workers(&mut app, i as u64)?;
        println!("{}", rs);
        if prev.is_some()
        && prev.as_ref().unwrap().messages_per_second >= rs.messages_per_second
        {
            break;
        } else {
            prev = Some(rs);
        }
    }

    match prev {
        Some(rs) => {
            println!("Found maximum:\n{}", rs);
        }
        None => {
            println!("No successful run");
        }
    }

    return Ok(())
}
