#ifndef PROMETHEUS_HPP
#define PROMETHEUS_HPP

#include <prometheus/gauge.h>
#include <prometheus/registry.h>
#include <prometheus/gateway.h>

#include "./config.cpp"

using namespace std;
using namespace prometheus;

class Prometheus {
  shared_ptr<Registry> registry;
  Gateway gateway;

public:
  Prometheus(const Config &c);
  Family<Gauge>& test_metric;
  void Push();
};

#endif
