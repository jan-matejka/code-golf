#ifndef PRIMITIVES_HPP
#define PRIMITIVES_HPP

#include <chrono>
#include <fmt/format.h>
#include <string>
#include <list>
#include <map>

#include <prometheus/labels.h>

#include "./log.hpp"

using namespace std;
using namespace fmt;

using WorkDuration = chrono::duration<double>;

class WorkerResult {
public:
  int WorkerId;
  int MessagesTotal;
  WorkDuration Duration;
  float DurationSeconds;
  float MessagesPerSecond;

  WorkerResult(int WorkerId, int MessagesTotal, WorkDuration Duration);
  operator string() const {
    stringstream ss;
    ss << "WorkerResult:"
      << " WorkerId=" << WorkerId
      << " MessagesTotal=" << MessagesTotal
      << " Duration=" << Duration
      << " MessagesPerSecond=" << MessagesPerSecond
    ;
    return ss.str();
  }
};

class Results {
public:
  list<WorkerResult> Workers;
  int MessagesTotal = 0;
  WorkDuration Duration = chrono::nanoseconds(0);
  float DurationSeconds = 0;
  float MessagesPerSecond = 0;

  void Add(WorkerResult wr);
  void Print(logger& log) const;
  operator string() const {
    stringstream ss;
    ss << "Results:"
      << " MessagesTotal=" << MessagesTotal
      << " Duration=" << Duration
      << " MessagesPerSecond=" << MessagesPerSecond
    ;
    return ss.str();
  }
};


template <> struct fmt::formatter<Results> : formatter<string_view> {
  auto format(const Results& rs, format_context& ctx) const
  -> format_context::iterator {
    auto s = (string)rs;
    return formatter<string_view>::format(s, ctx);
  }
};

inline bool operator<(const Results& lhs, const Results& rhs) {
  return lhs.MessagesPerSecond < rhs.MessagesPerSecond;
}

inline bool operator>(const Results& lhs, const Results& rhs) {
  return lhs.MessagesPerSecond > rhs.MessagesPerSecond;
}

inline bool operator==(const Results& lhs, const Results& rhs) {
  return (
    lhs.Duration == rhs.Duration
    && lhs.MessagesTotal == rhs.MessagesTotal
    && lhs.Workers.size() == rhs.Workers.size()
  );
}

class SampleDesc {
public:
  int n_workers;
  string algorithm;
  string mq_system;
  map<string,string> Map() const;
};

#endif
