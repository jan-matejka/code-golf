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

void PushTestMetric(Config &c) {
  INFO("Testing push to prometheus");

  auto rt = Runtime();
  INFO(string(rt));

  auto registry = std::make_shared<Registry>();
  auto& test_metric = BuildGauge()
    .Name("test")
    .Help("Test Metric")
    .Register(*registry);

  auto& test_metric_labeled = test_metric.Add(
    {{"worker_id", "0"}}
  );

  test_metric_labeled.Increment();

  auto job_name = string("mq-producer");
  auto port = to_string(c.prometheus_gateway_port);
  auto g = Gateway(
    c.prometheus_gateway_host,
    port,
    job_name
  );

  g.RegisterCollectable(registry);

  auto status = g.PushAdd();
  if (status != 200) {
    THROW("Prometheus push failed: " << status);
  }
}
