#include "postgres.hpp"
#include <chrono>

using namespace std;

Postgres::Postgres(Config &c)
: conn(connection(c.pg_results_dsn)) {
}

Postgres::~Postgres() {
  try {
    conn.disconnect();
  }catch(...) {}
}

void Postgres::CreateRuntime(const Runtime &r) {
  auto q = string(R"(
    insert into results.runtime (
        ctime, uuid, lang, lang_version, runtime, os, kernel, arch
    ) values (
        $1, $2, $3, $4, $5, $6, $7, $8
    )
    returning id;
  )");

  auto ctime = r.ctime_string();
  auto tx = work(conn);
  auto rs = tx.exec_params(
    q, ctime, to_string(r.uuid), r.lang, r.lang_version, r.runtime, r.os,
    r.kernel, r.arch
  );
  runtime_id.emplace(rs[0][0].as<int>());
  tx.commit();
}

int Postgres::CreateSample(const SampleDesc &s) {
  auto q = string(R"(
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
  )");

  auto tx = work(conn);
  auto rs = tx.exec_params(
    q, runtime_id.value(), s.n_workers, s.algorithm, s.mq_system
  );
  int id = rs[0][0].as<int>();
  tx.commit();
  return id;
}

void Postgres::CreateWorker(int sample_id, const WorkerResult &w) {
  auto q = string(R"(
    insert into results.worker
    (sample_id, worker_id, messages_total, duration_ns)
    values
    ($1, $2, $3, $4)
  )");

  auto tx = work(conn);
  auto ns = chrono::duration_cast<chrono::nanoseconds>(w.Duration).count();
  tx.exec_params(q, sample_id, w.WorkerId, w.MessagesTotal, ns);
  tx.commit();
}

void Postgres::Push(
  const Runtime& r, const SampleDesc& sdesc, const Results& rs
){
  if(!runtime_id.has_value()) {
    CreateRuntime(r);
  }

  int sample_id = CreateSample(sdesc);
  for(auto& wr : rs.Workers) {
    CreateWorker(sample_id, wr);
  }
}
