#ifndef LOG_CPP
#define LOG_CPP

#include "./log.hpp"

logger::logger
( FILE* out
)
: out(out)
{}

logger::logger()
: logger(stdout)
{}

void logger::info(string s) {
  print(s);
}

void logger::verbose(string s) {
  if(igetenv("VERBOSE", 0))
    print(s);
}

void logger::print(string s) {
  fprintf(out, "%s\n", s.c_str());
  if (ferror(out)) {
    fprintf(stderr, "%s\n", "Failed to write to log");
  }
}

logger logger::null() {
  auto o = fopen("/dev/null", "w");
  if (!o) {
    throw runtime_error("Failed to open /dev/null");
  }
  return logger(o);
}

#endif
