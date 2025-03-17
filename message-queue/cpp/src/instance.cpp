#include "./instance.hpp"

Instance::Instance()
: prometheus(telemetry::Prometheus(config)), pg(telemetry::Postgres(config))
{}

prometheus::Labels mk_labels(const Instance& app, const WorkerResult& wr, const SampleDesc& sdesc) {
  Labels labels = app.runtime.Map();
  labels.merge(sdesc.Map());
  labels.insert(pair{"worker_id", to_string(wr.WorkerId)});
  return labels;
}

void PushTestMetric(Instance& app) {
  INFO("Testing push to prometheus");
  INFO(string(app.runtime));

  auto labels = mk_labels(
    app,
    WorkerResult(0, 42, chrono::seconds(1)),
    SampleDesc(4, "threading", "postgres")
  );
  app.prometheus.test_metric.Add(labels).Increment();

  app.prometheus.Push();
}
