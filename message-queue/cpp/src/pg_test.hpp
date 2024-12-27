#ifndef PG_UNITTEST_CPP
#define PG_UNITTEST_CPP

#include <string>
#include <pqxx/pqxx>

using namespace std;
using namespace pqxx;

class TestConfig {
  TestConfig();
public:
  string PG_TEST_DSN;
  string PG_TEST_ROOT_DSN;
  string PG_TEST_MQ_DSN;
  static const TestConfig& Instance();
};

class PgFixture {
  connection root_conn;
  string CreateTestDb();

public:
  const string dsn;
  PgFixture();
  ~PgFixture();
};

#endif
