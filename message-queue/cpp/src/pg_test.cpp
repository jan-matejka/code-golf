#include "pg_test.hpp"
#include "log.hpp"
#include <fmt/format.h>

TestConfig::TestConfig()
: PG_TEST_DSN(sgetenv("PG_TEST_DSN", "localhost:5433"))
, PG_TEST_ROOT_DSN(fmt::format("postgres://postgres@{}", PG_TEST_DSN))
, PG_TEST_MQ_DSN(fmt::format("postgres://mq@{}/test", PG_TEST_DSN))
{
};

const TestConfig& TestConfig::Instance() {
  static TestConfig i;
  return i;
}

const TestConfig TCG();

PgFixture::PgFixture()
: root_conn(TestConfig::Instance().PG_TEST_ROOT_DSN),
  dsn(CreateTestDb())
{
};

string PgFixture::CreateTestDb() {
  nontransaction tx(root_conn);
  tx.exec("drop database if exists test");
  tx.exec("create database test template mq");
  return string(TestConfig::Instance().PG_TEST_MQ_DSN);
};

PgFixture::~PgFixture() {
  try {
    root_conn.disconnect();
  }catch(...) {}
}
