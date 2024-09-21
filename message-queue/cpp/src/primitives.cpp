#ifndef PRIMITIVES_CPP
#define PRIMITIVES_CPP

#include <chrono>
#include <string>

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
#endif
