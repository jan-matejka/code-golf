#ifndef PROMETHEUS_HPP
#define PROMETHEUS_HPP

#include <prometheus/gauge.h>
#include <prometheus/registry.h>
#include <prometheus/gateway.h>

#include "./config.cpp"
#include "./runtime.cpp"

using namespace std;
using namespace prometheus;

class Prometheus {
  shared_ptr<Registry> registry;
  Gateway gateway;

public:
  Prometheus(const Config &c);
  Family<Gauge>& test_metric;
  Family<Gauge>& messages_total;
  Family<Gauge>& messages_per_second;
  Family<Gauge>& duration_seconds;
  void Push();
};

#endif
