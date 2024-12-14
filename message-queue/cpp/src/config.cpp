#ifndef CONFIG_CPP
#define CONFIG_CPP

#include "./config.hpp"
#include "./log.hpp"

using namespace std;

Config::Config() {
  duration = igetenv("DURATION", 3);
  power = igetenv("POWER", 0);
  test_prometheus = igetenv("TEST_PROMETHEUS", 0);
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
