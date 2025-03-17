#ifndef MT_SYSTEM_THREAD_HPP
#define MT_SYSTEM_THREAD_HPP

#include <barrier>
#include <cmath>
#include <fmt/format.h>
#include <thread>
#include <mutex>
#include <chrono>
#include <queue>
#include <ranges>
#include <functional>
#include <memory>
#include <optional>

#include <boost/signals2/signal.hpp>
#include <boost/algorithm/string.hpp>
#include "../algorithm.hpp"
#include "../config.hpp"
#include "../runtime.hpp"
#include "../telemetry/prometheus.hpp"
#include "../log.hpp"
#include "../primitives.hpp"
#include "../instance.hpp"
#include "../mq_system/abc.hpp"

using namespace fmt;
using namespace std;
using namespace pqxx;
using namespace std::placeholders;
using namespace chrono;

class Worker {
  int worker_id;
  unique_ptr<mqs::abc::sender> sender;
  bool& exit;
  shared_ptr<queue<optional<WorkerResult>>>& result;
  bool barriers_passed = false;
  barrier<>& barr;
  mutex &mut;

  WorkerResult sample();
  void push(optional<WorkerResult> wr);

public:
  Worker(
    int worker_id,
    unique_ptr<mqs::abc::sender> s,
    bool& exit,
    shared_ptr<queue<optional<WorkerResult>>>& result,
    barrier<>& barr,
    mutex& mut
  );

  ~Worker();

  void operator()() {
    try {
      try {
        auto wr = sample();
        WVERBOSE(
          worker_id,
          fmt::format(
            "pushing {} into {}", wr.MessagesTotal, ptr(result.get())
          )
        );
        push(wr);
      } catch (const std::exception &e) {
        WERR(worker_id, e.what());
        throw;
      } catch (...) {
        WERR(worker_id, "Unknown exception");
        throw;
      }
    } catch (...) {
      push(nullopt);
    }
  }
};

class Observable {
public:
  boost::signals2::signal<void (Results)> sample_result;
};

// Sampler was made into template so we can use it with fake worker in tests.
// It didn't work out. I don't remember why. Anyway, I'm keeping it for now.
template<class W = Worker>
class Sampler {
  Instance& app;
  mqs::abc::mq& mq;
  function<void(milliseconds)> sleep_for;

public:
  Observable observable;

  Sampler(Instance&, mqs::abc::mq& mq);
  Sampler(
    Instance&
  , mqs::abc::mq&
  , function<void(milliseconds)>
  );
  optional<Results> run(int n);
};

template class Sampler<Worker>;

#endif
