#ifndef PRIMITIVES_CPP
#define PRIMITIVES_CPP

#include <chrono>

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
