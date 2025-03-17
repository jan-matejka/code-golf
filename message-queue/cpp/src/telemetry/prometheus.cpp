#ifndef TELEMETRY_PROMETHEUS_CPP
#define TELEMETRY_PROMETHEUS_CPP

#include "./prometheus.hpp"

namespace telemetry::prometheus {

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

Labels mk_labels(
  const Runtime& rt
, const WorkerResult& wr
, const SampleDesc& sdesc
) {
  Labels labels = rt.Map();
  labels.merge(sdesc.Map());
  labels.insert(pair{"worker_id", to_string(wr.WorkerId)});
  return labels;
}

void Prometheus::Push(
  const Runtime& rt
, const SampleDesc& sdesc
, const Results& rs
) {
  for(const auto& wr : rs.Workers) {
    messages_total
      .Add(mk_labels(rt, wr, sdesc))
      .Set(wr.MessagesTotal);

    messages_per_second
      .Add(mk_labels(rt, wr, sdesc))
      .Set(wr.MessagesPerSecond);

    duration_seconds
      .Add(mk_labels(rt, wr, sdesc))
      .Set(wr.DurationSeconds);
  }

  Push();
}

void Prometheus::Push() {
  auto status = gateway.PushAdd();
  if (status != 200) {
    THROW("Prometheus push failed: {}", status);
  }
}

void Prometheus::PushTest(const Runtime& rt) {
  INFO("Testing push to prometheus");
  INFO(string(rt));

  auto labels = mk_labels(
    rt,
    WorkerResult(0, 42, chrono::seconds(1)),
    SampleDesc(4, "threading", "postgres")
  );
  test_metric.Add(labels).Increment();
  Push();
}

}
#endif
