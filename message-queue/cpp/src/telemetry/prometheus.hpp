#ifndef TELEMETRY_PROMETHEUS_HPP
#define TELEMETRY_PROMETHEUS_HPP

#include <string>
#include <memory>
#include <boost/predef.h>

#include <prometheus/gauge.h>
#include <prometheus/registry.h>
#include <prometheus/gateway.h>

#include "../config.hpp"
#include "../log.hpp"
#include "../config.hpp"
#include "../primitives.hpp"

using namespace std;
using namespace prometheus;

namespace telemetry::prometheus {

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

}
#endif
