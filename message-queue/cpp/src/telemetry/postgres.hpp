#ifndef TELEMETRY_POSTGRES_HPP
#define TELEMETRY_POSTGRES_HPP

#include <pqxx/pqxx>

#include "../config.hpp"
#include "../runtime.hpp"
#include "../primitives.hpp"
#include "abc.hpp"

using namespace std;
using namespace pqxx;

namespace telemetry::postgres {

class Postgres : public telemetry::abc::pusher_abc {
  connection conn;
  optional<int> runtime_id = nullopt;
  void CreateRuntime(const Runtime&);
  int CreateSample(const SampleDesc&);
  void CreateWorker(int, const WorkerResult&);

public:
  Postgres(Config&);
  ~Postgres();
  void Push(const Runtime&, const SampleDesc&, const Results&);
};

}

#endif
