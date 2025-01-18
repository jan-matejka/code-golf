#ifndef PG_UNITTEST_CPP
#define PG_UNITTEST_CPP

#include <string>
#include <memory>

#include <pqxx/pqxx>

using namespace std;
using namespace pqxx;

class PgTestDb {
  connection root_conn;
  void CreateTestDb();

public:
  const string dsn;
  PgTestDb(const string&, const string&);
  ~PgTestDb();
};

class TestConfig {
  TestConfig();
public:
  string PG_TEST_DSN;
  string PG_TEST_ROOT_DSN;
  string PG_TEST_MQ_DSN;
  string TEST_TELEMETRY_POSTGRES;
  string TEST_TELEMETRY_POSTGRES_ROOT;
  string TEST_TELEMETRY_POSTGRES_MQ;
  static const TestConfig& Instance();
  unique_ptr<PgTestDb> telemetry_postgres();
  unique_ptr<PgTestDb> mq_postgres();
};

#endif
