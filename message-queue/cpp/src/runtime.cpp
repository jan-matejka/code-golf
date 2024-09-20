#ifndef RUNTIME_CPP
#define RUNTIME_CPP

#include <ctime>
#include <errno.h>
#include <string.h>
#include <sys/utsname.h>

#include <chrono>

#include <boost/predef.h>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "./log.cpp"
#include "./config.cpp"
#include "./prometheus.hpp"

using namespace std;

class Runtime {
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

  string ctime_string() const {
    // TBD: this is a mess without <format>, figure out later
    time_t _ctime = chrono::system_clock::to_time_t(ctime);
    auto _ctime2 = localtime(&_ctime);
    char buff[32];
    strftime(data(buff), 32, "%F %T", _ctime2);
    auto s = string(buff);
    return s;
  }

  operator std::string() const {
    stringstream ss;
    ss << "Runtime:";
    for(const auto& [k, v] : Map()) {
      ss << " " << k << "=" << v;
    }
    return ss.str();
  }
};

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
    {"lang_version", lang_version},
    {"runtime", runtime},
    {"os", os},
    {"kernel", kernel},
    {"arch", arch},
  };
  return xs;
}

class Instance {
public:
  Config config;
  Runtime runtime;
  Prometheus prometheus;
  Instance();
};

Instance::Instance() : prometheus(Prometheus(config)) {}

#endif
