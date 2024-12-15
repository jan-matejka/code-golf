#include "pg_unittest.hpp"

PgFixture::PgFixture()
: root_conn("postgres://postgres@localhost:5433"),
  dsn(CreateTestDb())
{
};

string PgFixture::CreateTestDb() {
  nontransaction tx(root_conn);
  tx.exec("drop database if exists test");
  tx.exec("create database test template mq");
  return string("postgres://mq@localhost:5433/test");
};

PgFixture::~PgFixture() {
  try {
    root_conn.disconnect();
  }catch(...) {}
}
