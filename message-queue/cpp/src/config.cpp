#ifndef CONFIG_CPP
#define CONFIG_CPP

#include "./config.hpp"
#include "./log.hpp"

using namespace std;

Config::Config(const string& telemetry_postgres)
{
  duration = igetenv("DURATION", 3);
  power = igetenv("POWER", 0);
  test_prometheus = igetenv("TEST_PROMETHEUS", 0);
  this->telemetry_postgres = telemetry_postgres;
}

Config::Config()
: Config(sgetenv(
    "TELEMETRY_POSTGRES", string("postgres://mq@localhost:5442/mq")
)) {
}

string Config::str() {
  stringstream s;
  s
    << "duration=" << duration
    << " power=" << power
    << " test_prometheus=" << test_prometheus
    << " telemetry_postgres=" << telemetry_postgres
  ;
  return s.str();
}

#endif
