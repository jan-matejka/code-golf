#include <gtest/gtest.h>

#include "postgres.hpp"
#include "pg_test.hpp"
#include "config.hpp"

namespace {
TEST(Results, Push) {
  auto tcg = TestConfig::Instance();

  SampleDesc sdesc(2, "threading", "postgres");
  Results rs;
  rs.Add(WorkerResult(1, 10, chrono::duration<double>(1s)));
  rs.Add(WorkerResult(2, 20, chrono::duration<double>(2s)));

  auto pgf = tcg.telemetry_postgres();

  Config cg(pgf->dsn);
  Postgres pg(cg);
  Runtime r;

  pg.Push(r, sdesc, rs);

  connection conn(pgf->dsn);
  work tx(conn);
  auto pq_runtime = tx.exec(R"(
  select
    id,ctime,uuid,lang,lang_version,runtime,os,kernel,arch
  from results.runtime
  )");
  ASSERT_EQ(size(pq_runtime), 1);
  ASSERT_EQ(pq_runtime[0][1].as<string>(), r.ctime_string());
  ASSERT_EQ(pq_runtime[0][2].as<string>(), to_string(r.uuid));
  ASSERT_EQ(pq_runtime[0][3].as<string>(), r.lang);
  ASSERT_EQ(pq_runtime[0][4].as<string>(), r.lang_version);
  ASSERT_EQ(pq_runtime[0][5].as<string>(), r.runtime);
  ASSERT_EQ(pq_runtime[0][6].as<string>(), r.os);
  ASSERT_EQ(pq_runtime[0][7].as<string>(), r.kernel);
  ASSERT_EQ(pq_runtime[0][8].as<string>(), r.arch);

  auto pq_sample = tx.exec(R"(
  select
    id,runtime_id,n_workers,algorithm,mq_system
  from results.sample
  )");
  ASSERT_EQ(size(pq_sample), 1);
  ASSERT_EQ(pq_sample[0][1].as<int>(), pq_runtime[0][0].as<int>());
  ASSERT_EQ(pq_sample[0][2].as<int>(), sdesc.n_workers);
  ASSERT_EQ(pq_sample[0][3].as<string>(), sdesc.algorithm);
  ASSERT_EQ(pq_sample[0][4].as<string>(), sdesc.mq_system);

  auto pq_workers = tx.exec(R"(
  select
    id,sample_id,worker_id,messages_total,duration_ns
  from results.worker
  )");
  ASSERT_EQ(size(pq_workers), 2);
  ASSERT_EQ(pq_workers[0][1].as<int>(), pq_sample[0][0].as<int>());
  ASSERT_EQ(pq_workers[0][2].as<int>(), 1);
  ASSERT_EQ(pq_workers[0][3].as<int>(), 10);
  ASSERT_EQ(pq_workers[0][4].as<int>(), 1'000'000'000);

  ASSERT_EQ(pq_workers[1][1].as<int>(), pq_sample[0][0].as<int>());
  ASSERT_EQ(pq_workers[1][2].as<int>(), 2);
  ASSERT_EQ(pq_workers[1][3].as<int>(), 20);
  ASSERT_EQ(pq_workers[1][4].as<int>(), 2'000'000'000);

  sdesc.n_workers = 4;
  pg.Push(r, sdesc, rs);

  pq_runtime = tx.exec(R"(
  select
    id,ctime,uuid,lang,lang_version,runtime,os,kernel,arch
  from results.runtime
  )");
  ASSERT_EQ(size(pq_runtime), 1);

  pq_sample = tx.exec(R"(
  select
    id,runtime_id,n_workers,algorithm,mq_system
  from results.sample
  )");
  ASSERT_EQ(size(pq_sample), 2);
}
}
