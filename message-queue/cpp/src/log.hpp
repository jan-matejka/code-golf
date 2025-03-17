#ifndef LOG_HPP
#define LOG_HPP

#include <fmt/format.h>
#include <fmt/os.h>
#include <utility>
#include <sstream>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <boost/algorithm/string.hpp>

#define VERBOSE(x) if(igetenv("VERBOSE", 0)) fmt::print("{}\n", x)
#define INFO(x) fmt::print("{}\n", x)
#define ERR(x) fmt::print(stderr, "{}\n", x)
#define WVERBOSE(id, x) VERBOSE(fmt::format("Worker {}: {}", id, x))
#define WERR(id, x) ERR(fmt::format("Worker {}: ", id, x))
#define THROW(f, args...) { throw runtime_error(fmt::format(f, args)); }

using namespace fmt;
using namespace std;

inline int igetenv(const char* x, int def) {
  char *c = getenv(x);
  if(!c)
    return def; // x undefined

  string s(c);
  boost::trim(s);
  if (s.empty()) // x defined empty
    return def;

  // x defined with some value
  try {
    int i = stoi(s);
    return i;
  }catch(const invalid_argument& e) {
    stringstream s;
    s << "invalid environemnt variable: " << x;
    throw runtime_error(s.str());
  }catch(const out_of_range& e) {
    stringstream s;
    s << "out of bounds environment variable:" << x;
    throw runtime_error(s.str());
  }
}

inline string sgetenv(const char *x, const string def) {
  char *c = getenv(x);
  if(!c)
    return def; // x undefined

  string s(c);
  boost::trim(s);
  if (s.empty()) // x defined empty
    return def;

  return s;
}

class logger {
  FILE* out;
protected:
  void print(string);
public:
  logger();
  logger(FILE*);
  static logger null();

  void info(string s);
  void verbose(string s);
};

#endif
