#ifndef PRIMITIVES_CPP
#define PRIMITIVES_CPP

#include <chrono>
#include <string>
#include <list>
#include <map>

#include <prometheus/labels.h>

#include "./log.cpp"

using namespace std;

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

WorkerResult::WorkerResult(int WorkerId, int MessagesTotal, WorkDuration Duration)
: WorkerId(WorkerId)
, MessagesTotal(MessagesTotal)
, Duration(Duration)
, DurationSeconds(Duration.count())
, MessagesPerSecond(MessagesTotal / DurationSeconds)
{
}

class Results {
public:
  list<WorkerResult> Workers;
  int MessagesTotal = 0;
  WorkDuration Duration = chrono::nanoseconds(0);
  float DurationSeconds = 0;
  float MessagesPerSecond = 0;

  void Add(WorkerResult wr);
  void Print() const;
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

void Results::Add(WorkerResult wr) {
  Workers.push_back(wr);
  MessagesTotal += wr.MessagesTotal;
  Duration += wr.Duration;

  DurationSeconds = Duration.count();
  MessagesPerSecond = MessagesTotal / DurationSeconds;
}

void Results::Print() const {
  for(auto& wr : Workers) {
    INFO(string(wr));
  }

  INFO("Total: " << MessagesTotal);
  INFO("Total mps: " << MessagesPerSecond);
}

class SampleDesc {
public:
  int n_workers;
  string algorithm;
  string mq_system;
  map<string,string> Map() const;
};

map<string,string> SampleDesc::Map() const {
  map<string,string> xs = {
    {"n_workers", to_string(n_workers)},
    {"algorithm", algorithm},
    {"mq_system", mq_system},
  };
  return xs;
}

#endif
