#ifndef RUNTIME_CPP
#define RUNTIME_CPP

#include "./runtime.hpp"

string Runtime::ctime_string() const {
  // TBD: this is a mess without <format>, figure out later
  time_t _ctime = chrono::system_clock::to_time_t(ctime);
  auto _ctime2 = localtime(&_ctime);
  char buff[32];
  strftime(data(buff), 32, "%F %T", _ctime2);
  auto s = string(buff);
  return s;
}

string _ver(int major, int minor, int patch) {
  ostringstream ss;
  ss << major << '.' << minor << '.' << patch;
  return ss.str();
}

string _kernel() {
  utsname unam;
  if (uname(&unam) == -1) {
    int errsv = errno;
    auto err = string(strerror(errsv));
    THROW("utsname failed: " << errsv << ": " << err);
  }
  return string(unam.release);
}

string _arch() {
  utsname unam;
  if (uname(&unam) == -1) {
    int errsv = errno;
    auto err = string(strerror(errsv));
    THROW("utsname failed: " << errsv << ": " << err);
  }
  return string(unam.machine);
}


Runtime::Runtime()
: ctime(chrono::system_clock::now())
, uuid(boost::uuids::random_generator()())
, lang(string("c++"))
, lang_version(string("c++23")) // figure this out later, looks messy
, runtime(BOOST_COMPILER)
, os(BOOST_PLATFORM)
, kernel(_kernel())
, arch(_arch())
{
}

map<string,string> Runtime::Map() const {
  map<string,string> xs = {
    {"ctime", ctime_string()},
    {"uuid", to_string(uuid)},
    {"lang", lang},
    {"lang_version", lang_version},
    {"runtime", runtime},
    {"os", os},
    {"kernel", kernel},
    {"arch", arch},
  };
  return xs;
}

Instance::Instance() : prometheus(Prometheus(config)) {}

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

#endif
