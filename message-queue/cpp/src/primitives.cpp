#ifndef PRIMITIVES_CPP
#define PRIMITIVES_CPP
#include "./primitives.hpp"

WorkerResult::WorkerResult(int WorkerId, int MessagesTotal, WorkDuration Duration)
: WorkerId(WorkerId)
, MessagesTotal(MessagesTotal)
, Duration(Duration)
, DurationSeconds(Duration.count())
, MessagesPerSecond(MessagesTotal / DurationSeconds)
{
}

void Results::Add(WorkerResult wr) {
  Workers.push_back(wr);
  MessagesTotal += wr.MessagesTotal;
  Duration += wr.Duration;

  DurationSeconds = Duration.count();
  MessagesPerSecond = Workers.size() * MessagesTotal / DurationSeconds;
}

void Results::Print(logger& log) const {
  for(auto& wr : Workers) {
    log.info(string(wr));
  }

  auto s = fmt::format("Total: {}", MessagesTotal);
  log.info(s);
  s = fmt::format("Total mps: {}", MessagesPerSecond);
  log.info(s);
}

map<string,string> SampleDesc::Map() const {
  map<string,string> xs = {
    {"n_workers", to_string(n_workers)},
    {"algorithm", algorithm},
    {"mq_system", mq_system},
  };
  return xs;
}

#endif
