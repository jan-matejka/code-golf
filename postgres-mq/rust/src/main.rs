use postgres as pg;
use std::thread;

fn insert(i: u64) -> Result<(),pg::Error> {
    let mut client = pg::Client::connect("postgres://mq@localhost/mq", pg::NoTls)?;

    let i_sql = i.to_string();
    client.execute("insert into queue (data) values ($1)", &[&i_sql])?;
    return Ok(())
}

fn worker() -> Result<(), pg::Error> {
    insert(1)?;
    return Ok(());
}

fn main() -> Result<(), pg::Error> {
    let mut handles = Vec::new();
    for _ in 1..10 {
        let h = thread::spawn(worker);
        handles.push(h);
    }

    for v in handles {
        v.join().unwrap()?;
    }

    return Ok(());
}
