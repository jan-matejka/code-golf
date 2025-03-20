#ifndef INSTANCE_HPP
#define INSTANCE_HPP

#include <prometheus/labels.h>

#include "./config.hpp"
#include "./primitives.hpp"
#include "./telemetry.hpp"
#include "./runtime.hpp"

using namespace std;

class Instance {
public:
  Config config;
  Runtime runtime;
  telemetry::Prometheus prometheus;
  telemetry::Postgres pg;
  Instance();
};

#endif
