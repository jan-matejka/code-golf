#ifndef POSTGRES_CPP
#define POSTGRES_CPP

#include <optional>

#include <pqxx/pqxx>

#include "config.hpp"
#include "runtime.hpp"
#include "primitives.hpp"

using namespace std;
using namespace pqxx;

class Postgres {
  connection conn;
  optional<int> runtime_id = nullopt;

public:
  Postgres(Config&);
  void Push(Runtime&, SampleDesc&, Results&);
};

#endif
