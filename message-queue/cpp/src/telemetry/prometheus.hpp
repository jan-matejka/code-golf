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
#include "abc.hpp"

using namespace std;
using namespace prometheus;

namespace telemetry::prometheus {

Labels mk_labels(
  const Runtime&
, const WorkerResult&
, const SampleDesc&
);

class Prometheus : public telemetry::abc::pusher_abc {
  shared_ptr<Registry> registry;
  Gateway gateway;

public:
  Prometheus(const Config &c);
  Family<Gauge>& test_metric;
  Family<Gauge>& messages_total;
  Family<Gauge>& messages_per_second;
  Family<Gauge>& duration_seconds;
  /**
   * Set the metrics and Push().
   */
  void Push(const Runtime&, const SampleDesc&, const Results&);
  /**
   * Actually push the collected metrics into the push gateway.
   */
  void Push();

  /**
   * Push test_metric
   */
  void PushTest(const Runtime& rt);
};

}
#endif
