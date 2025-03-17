#include "./instance.hpp"

Instance::Instance()
: prometheus(telemetry::Prometheus(config)), pg(telemetry::Postgres(config))
{}
