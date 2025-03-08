#ifndef INSTANCE_HPP
#define INSTANCE_HPP

#include <prometheus/labels.h>

#include "./config.hpp"
#include "./primitives.hpp"
#include "./telemetry/postgres.hpp"
#include "./telemetry/prometheus.hpp"
#include "./runtime.hpp"

using namespace std;

class Instance {
public:
  Config config;
  Runtime runtime;
  Prometheus prometheus;
  Postgres pg;
  Instance();
};

prometheus::Labels mk_labels(const Instance& app, const WorkerResult& wr, const SampleDesc& sdesc);

void PushTestMetric(Instance& app);
#endif
