#ifndef CONFIG_CPP
#define CONFIG_CPP
#include "./log.cpp"

using namespace std;

class Config {
public:
  int duration = 3;
  int power = 0;
  int test_prometheus = 0;
  string prometheus_gateway_host = "localhost";
  int prometheus_gateway_port = 9091;

  Config() {
    duration = igetenv("DURATION", 3);
    power = igetenv("POWER", 0);
    test_prometheus = igetenv("TEST_PROMETHEUS", 0);
  }

  string str() {
    stringstream s;
    s
      << "duration=" << duration
      << " power=" << power
      << " test_prometheus=" << test_prometheus;
    return s.str();
  }
};

#endif
