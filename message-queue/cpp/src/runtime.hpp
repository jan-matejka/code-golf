#ifndef RUNTIME_HPP
#define RUNTIME_HPP

#include <ctime>
#include <errno.h>
#include <string.h>
#include <sys/utsname.h>

#include <chrono>

#include <boost/predef.h>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include <prometheus/labels.h>

#include "./log.hpp"
#include "./config.hpp"
#include "./primitives.hpp"
#include "./prometheus.hpp"

using namespace std;

class Runtime {
  string ctime_string() const;
public:
  const chrono::time_point<chrono::system_clock> ctime;
  const boost::uuids::uuid uuid;
  const string lang;
  const string lang_version;
  const string runtime;
  const string os;
  const string kernel;
  const string arch;
  Runtime();
  map<string,string> Map() const;

  operator std::string() const {
    stringstream ss;
    ss << "Runtime:";
    for(const auto& [k, v] : Map()) {
      ss << " " << k << "=" << v;
    }
    return ss.str();
  }
};

class Instance {
public:
  Config config;
  Runtime runtime;
  Prometheus prometheus;
  Instance();
};

prometheus::Labels mk_labels(const Instance& app, const WorkerResult& wr, const SampleDesc& sdesc);

void PushTestMetric(Instance& app);
#endif
