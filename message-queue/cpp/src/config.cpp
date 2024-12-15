#ifndef CONFIG_CPP
#define CONFIG_CPP

#include "./config.hpp"
#include "./log.hpp"

using namespace std;

Config::Config(const string& pg_results_dsn)
{
  duration = igetenv("DURATION", 3);
  power = igetenv("POWER", 0);
  test_prometheus = igetenv("TEST_PROMETHEUS", 0);
  this->pg_results_dsn = pg_results_dsn;
}

Config::Config()
: Config(sgetenv(
    "PG_RESULTS_DSN", string("postgres://mq@localhost:5432/mq")
)) {
}

string Config::str() {
  stringstream s;
  s
    << "duration=" << duration
    << " power=" << power
    << " test_prometheus=" << test_prometheus;
  return s.str();
}

#endif
