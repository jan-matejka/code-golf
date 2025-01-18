#include "pg_test.hpp"
#include "log.hpp"
#include <fmt/format.h>

TestConfig::TestConfig()
: PG_TEST_DSN(sgetenv("PG_TEST_DSN", "localhost:5433"))
, PG_TEST_ROOT_DSN(fmt::format("postgres://postgres@{}", PG_TEST_DSN))
, PG_TEST_MQ_DSN(fmt::format("postgres://mq@{}/test", PG_TEST_DSN))
, TEST_TELEMETRY_POSTGRES(sgetenv("TEST_TELEMETRY_POSTGRES", "localhost:5443"))
, TEST_TELEMETRY_POSTGRES_ROOT(fmt::format(
  "postgres://postgres@{}", TEST_TELEMETRY_POSTGRES
  ))
, TEST_TELEMETRY_POSTGRES_MQ(fmt::format(
  "postgres://mq@{}/test", TEST_TELEMETRY_POSTGRES
  ))
{
};

const TestConfig& TestConfig::Instance() {
  static TestConfig i;
  return i;
}

const TestConfig TCG();

unique_ptr<PgTestDb> TestConfig::telemetry_postgres(){
  auto x = unique_ptr<PgTestDb>(new PgTestDb(
    TEST_TELEMETRY_POSTGRES_ROOT, TEST_TELEMETRY_POSTGRES_MQ
  ));
  return x;
};

unique_ptr<PgTestDb> TestConfig::mq_postgres(){
  auto x = unique_ptr<PgTestDb>(new PgTestDb(
    PG_TEST_ROOT_DSN, PG_TEST_MQ_DSN
  ));
  return x;
};

PgTestDb::PgTestDb(const string& dsn, const string& mq_dsn)
: root_conn(dsn),
  dsn(mq_dsn)
{
  CreateTestDb();
};

void PgTestDb::CreateTestDb() {
  nontransaction tx(root_conn);
  tx.exec("drop database if exists test");
  tx.exec("create database test template mq");
};

PgTestDb::~PgTestDb() {
  try {
    root_conn.disconnect();
  }catch(...) {}
}

