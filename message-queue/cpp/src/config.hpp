#ifndef CONFIG_HPP
#define CONFIG_HPP

#include <string>

using namespace std;

class Config {
public:
  int duration = 3;
  int power = 0;
  int test_prometheus = 0;
  string prometheus_gateway_host = "localhost";
  int prometheus_gateway_port = 9091;

  Config();
  string str();
};

#endif
