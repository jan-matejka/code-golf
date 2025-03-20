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
  operator string() {
    stringstream ss;
    ss << "Results:"
      << " MessagesTotal=" << MessagesTotal
      << " Duration=" << Duration
      << " MessagesPerSecond=" << MessagesPerSecond
    ;
    return ss.str();
  }
};

class SampleDesc {
public:
  int n_workers;
  string algorithm;
  string mq_system;
  map<string,string> Map() const;
};

#endif
