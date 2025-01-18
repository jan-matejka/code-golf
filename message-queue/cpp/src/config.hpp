#ifndef CONFIG_HPP
#define CONFIG_HPP

#include <string>

using namespace std;

class Config {
public:
  int duration;
  int power;
  int test_prometheus;
  string prometheus_gateway_host = "localhost";
  int prometheus_gateway_port = 9091;
  string pg_results_dsn;

  Config(const string&);
  Config();
  string str();
};

#endif
