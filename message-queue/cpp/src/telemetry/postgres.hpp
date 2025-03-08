#ifndef TELEMETRY_POSTGRES_HPP
#define TELEMETRY_POSTGRES_HPP

#include <optional>

#include <pqxx/pqxx>

#include "../config.hpp"
#include "../runtime.hpp"
#include "../primitives.hpp"

using namespace std;
using namespace pqxx;

class Postgres {
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

#endif
