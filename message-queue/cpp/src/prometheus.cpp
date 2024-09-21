#ifndef PROMETHEUS_CPP
#define PROMETHEUS_CPP
#include <string>
#include <memory>

#include <prometheus/gauge.h>
#include <prometheus/registry.h>
#include <prometheus/gateway.h>

#include <boost/predef.h>
#include "./log.cpp"
#include "./config.cpp"
#include "./runtime.cpp"

using namespace std;
using namespace prometheus;

void PushTestMetric(Instance &app) {
  INFO("Testing push to prometheus");
  INFO(string(app.runtime));

  Labels labels = app.runtime.Map();
  Labels extra = {{"worker_id", "0"}};
  labels.merge(extra);
  app.prometheus.test_metric.Add(labels).Increment();

  app.prometheus.Push();
}

Gateway mk_gateway(const Config& c) {
  auto job_name = string("mq-producer");
  return Gateway(
    c.prometheus_gateway_host,
    to_string(c.prometheus_gateway_port),
    job_name
  );
}

#include "prometheus.hpp"

Prometheus::Prometheus(const Config &c)
: registry(std::make_shared<Registry>())
, gateway(mk_gateway(c))
, test_metric(
    BuildGauge()
    .Name("test")
    .Help("Test Metric")
    .Register(*registry)
  )
{
  gateway.RegisterCollectable(registry);
}

void Prometheus::Push() {
  auto status = gateway.PushAdd();
  if (status != 200) {
    THROW("Prometheus push failed: " << status);
  }
}

#endif
