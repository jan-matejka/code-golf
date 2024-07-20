use postgres::{Client, NoTls, Error};

fn insert(i: u64) -> Result<(),Error> {
    let mut client = Client::connect("postgres://mq@localhost/mq", NoTls)?;

    let i_sql = i.to_string();
    client.execute("insert into queue (data) values ($1)", &[&i_sql])?;
    return Ok(())
}
fn main() -> Result<(), Error> {
    insert(1)?;
    return Ok(());
}
