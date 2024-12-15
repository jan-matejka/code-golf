#ifndef PG_UNITTEST_CPP
#define PG_UNITTEST_CPP

#include <pqxx/pqxx>

using namespace std;
using namespace pqxx;

class PgFixture {
  connection root_conn;
  string CreateTestDb();

public:
  const string dsn;
  PgFixture();
  ~PgFixture();
};

#endif
