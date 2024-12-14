#ifndef PROMETHEUS_CPP
#define PROMETHEUS_CPP

#include "./prometheus.hpp"

Gateway mk_gateway(const Config& c) {
  auto job_name = string("mq-producer");
  return Gateway(
    c.prometheus_gateway_host,
    to_string(c.prometheus_gateway_port),
    job_name
  );
}

Prometheus::Prometheus(const Config &c)
: registry(std::make_shared<Registry>())
, gateway(mk_gateway(c))
, test_metric(
    BuildGauge()
    .Name("test")
    .Help("Test Metric")
    .Register(*registry)
  )
, messages_total(
    BuildGauge()
    .Name("messages_total")
    .Help("Messages sent")
    .Register(*registry)
  )
, messages_per_second(
    BuildGauge()
    .Name("messages_per_second")
    .Help("Messages per second sent")
    .Register(*registry)
  )
, duration_seconds(
    BuildGauge()
    .Name("duration_seconds")
    .Help("Work duration in seconds")
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
