#ifndef LOG_HPP
#define LOG_HPP

#include <iostream>
#include <syncstream>
#include <sstream>
#include <boost/algorithm/string.hpp>

#define VERBOSE(x) if(igetenv("VERBOSE", 0)) osyncstream(cout) << x << endl
#define INFO(x) osyncstream(cout) << x << endl
#define ERR(x) osyncstream(cerr) << x << endl
#define WVERBOSE(id, x) VERBOSE("Worker " << id << ": " << x)
#define WERR(id, x) ERR("Worker " << id << ": " << x)
#define THROW(x) { stringstream ss; ss << x; throw runtime_error(ss.str()); }

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

#endif
