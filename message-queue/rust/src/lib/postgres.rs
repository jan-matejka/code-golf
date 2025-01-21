use std::error;

use postgres as pg;

use crate::{Config, Runtime, Results, SampleDesc, WorkerResult};

pub struct Postgres {
    client: pg::Client,
    runtime_id: Option<i32>,
}

impl Postgres {
    pub fn new(cg: &Config) -> Result<Self,Box<dyn error::Error>> {
        let pg = Self{
            client: pg::Client::connect(&cg.pg_telemetry_dsn, pg::NoTls)?,
            runtime_id: None,
        };
        return Ok(pg);
    }

    fn create_runtime(&mut self, r: &Runtime) -> Result<(), Box<dyn error::Error>> {
        let q = "
        insert into results.runtime (
          ctime, uuid, lang, lang_version, runtime, os, kernel, arch
        ) values (
          $1, $2, $3, $4, $5, $6, $7, $8
        )
        returning id;
        ";
        let row = self.client.query_one(
            q,
            &[
                &r.ctime.naive_local(),
                &r.uuid.to_string(),
                &r.lang,
                &r.lang_version.to_string(),
                &r.runtime_string(),
                &r.os,
                &r.kernel,
                &r.arch
            ]
        )?;

        let id = row.get::<&str, i32>("id");
        let _ = self.runtime_id.insert(id);
        Ok(())
    }

    fn create_sample(&mut self, sdesc: &SampleDesc) -> Result<i32, Box<dyn error::Error>> {
        let q = "
        with
        sel as (
          select id from results.sample where
              runtime_id = $1
              and n_workers = $2
              and algorithm = $3
              and mq_system = $4
        ),
        ins as (
          insert into results.sample
          (runtime_id, n_workers, algorithm, mq_system)
          values
          ($1, $2, $3, $4)
          on conflict do nothing
          returning id
        )
        select * from ins
        union
        select * from sel
        where id is not null;
        ";
        let row = self.client.query_one(
            q,
            &[
                &self.runtime_id,
                &(sdesc.n_workers as i32),
                &sdesc.algorithm,
                &sdesc.mq_system
            ]
        )?;

        return Ok(row.get::<&str, i32>("id"));
    }

    pub fn push(&mut self, r: &Runtime, sdesc: &SampleDesc, rs: &Results) -> Result<(), Box<dyn error::Error>> {
        if self.runtime_id.is_none() {
            self.create_runtime(r)?;
        }

        let sample_id = self.create_sample(sdesc)?;
        for w in &rs.workers {
            self.create_worker(sample_id, w)?;
        }

        Ok(())
    }

    fn create_worker(&mut self, sample_id: i32, w: &WorkerResult) -> Result<(), Box<dyn error::Error>> {
        let q = "
        insert into results.worker
        (sample_id, worker_id, messages_total, duration_ns)
        values
        ($1, $2, $3, $4)
        ";
        self.client.execute(
            q,
            &[
                &sample_id,
                &(w.worker_id as i32),
                &(w.messages_total as i32),
                &(w.duration.as_nanos() as i64),
            ]
        )?;

        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{Duration};
    use uuid::Uuid;
    use crate::config::tests::{TCG};

    struct PgFixture {
        c: pg::Client
    }

    impl PgFixture {
        pub fn new() -> Result<Self, Box<dyn error::Error>> {
            let mut c = pg::Client::connect(&TCG.telemetry_pg_root, pg::NoTls)?;
            let _ = c.execute("drop database if exists test", &[]);
            _ = c.execute("create database test template mq", &[]);

            let t = pg::Client::connect(&TCG.telemetry_pg_mq, pg::NoTls)?;
            return Ok(Self{c: t});
        }
    }

    #[test]
    fn test_push() -> Result<(), Box<dyn error::Error>> {
        let cg = Config::new(
            Some(TCG.telemetry_pg_mq.to_string())
        )?;
        let mut pgf = PgFixture::new()?;
        let mut pg = Postgres::new(&cg)?;

        let mut ws = Vec::<WorkerResult>::new();
        ws.push(WorkerResult::new(1, 10, Duration::new(1, 0)));
        ws.push(WorkerResult::new(2, 20, Duration::new(2, 0)));
        let results = Results::new(ws);
        let mut sdesc = SampleDesc::new(2, "foo", "bar");
        let runtime = Runtime::new()?;
        pg.push(&runtime, &sdesc, &results)?;

        let mut q = "
        select
          id,ctime,uuid,lang,lang_version,runtime,os,kernel,arch
        from results.runtime
        ";
        let mut rs = pgf.c.query(q, &[])?;
        assert_eq!(rs.len(), 1);
        let runtime_id = rs[0].get::<&str,i32>("id");
        assert_eq!(
            rs[0].get::<&str,chrono::NaiveDateTime>("ctime").and_utc().timestamp_micros(),
            runtime.ctime.naive_local().and_utc().timestamp_micros()
        );
        assert_eq!(
            Uuid::parse_str(&rs[0].get::<&str,String>("uuid"))?,
            runtime.uuid
        );
        assert_eq!(rs[0].get::<&str,String>("lang"), runtime.lang);
        assert_eq!(
            rs[0].get::<&str,String>("lang_version"),
            runtime.lang_version.to_string()
        );
        assert_eq!(
            rs[0].get::<&str,String>("runtime"),
            runtime.runtime_string()
        );
        assert_eq!(rs[0].get::<&str,String>("os"), runtime.os);
        assert_eq!(rs[0].get::<&str,String>("kernel"), runtime.kernel);
        assert_eq!(rs[0].get::<&str,String>("arch"), runtime.arch);

        q = "
        select
            id,runtime_id,n_workers,algorithm,mq_system
        from results.sample
        ";
        rs = pgf.c.query(q, &[])?;
        assert_eq!(rs.len(), 1);
        let sample_id = rs[0].get::<&str,i32>("id");
        assert_eq!(rs[0].get::<&str,i32>("runtime_id"), runtime_id);
        assert_eq!(rs[0].get::<&str,i32>("n_workers") as u64, sdesc.n_workers);
        assert_eq!(rs[0].get::<&str,String>("algorithm"), sdesc.algorithm);
        assert_eq!(rs[0].get::<&str,String>("mq_system"), sdesc.mq_system);

        q = "
        select
            id,sample_id,worker_id,messages_total,duration_ns
        from results.worker
        ";
        rs = pgf.c.query(q, &[])?;
        assert_eq!(rs.len(), 2);
        assert_eq!(rs[0].get::<&str,i32>("sample_id"), sample_id);
        assert_eq!(
            rs[0].get::<&str,i32>("messages_total") as u64,
            results.workers[0].messages_total
        );
        assert_eq!(
            Duration::from_nanos(rs[0].get::<&str,i64>("duration_ns") as u64),
            results.workers[0].duration
        );

        assert_eq!(rs[1].get::<&str,i32>("sample_id"), sample_id);
        assert_eq!(
            rs[1].get::<&str,i32>("messages_total") as u64,
            results.workers[1].messages_total
        );
        assert_eq!(
            Duration::from_nanos(rs[1].get::<&str,i64>("duration_ns") as u64),
            results.workers[1].duration
        );

        sdesc.n_workers = 4;
        pg.push(&runtime, &sdesc, &results)?;

        let mut q = "
        select
          id,ctime,uuid,lang,lang_version,runtime,os,kernel,arch
        from results.runtime
        ";
        let mut rs = pgf.c.query(q, &[])?;
        assert_eq!(rs.len(), 1);

        q = "
        select
            id,runtime_id,n_workers,algorithm,mq_system
        from results.sample
        ";
        rs = pgf.c.query(q, &[])?;
        assert_eq!(rs.len(), 2);

        q = "
        select
            id,sample_id,worker_id,messages_total,duration_ns
        from results.worker
        ";
        rs = pgf.c.query(q, &[])?;
        assert_eq!(rs.len(), 4);

        Ok(())
    }
}
