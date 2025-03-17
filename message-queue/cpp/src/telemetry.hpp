#ifndef TELEMETRY_HPP
#define TELEMETRY_HPP

#include "./telemetry/postgres.hpp"
#include "./telemetry/prometheus.hpp"

namespace telemetry {
using postgres::Postgres;
using prometheus::Prometheus;
}

#endif
