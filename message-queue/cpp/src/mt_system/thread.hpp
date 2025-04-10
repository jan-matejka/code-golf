#ifndef MT_SYSTEM_THREAD_HPP
#define MT_SYSTEM_THREAD_HPP

#include <barrier>
#include <cmath>
#include <fmt/format.h>
#include <iostream>
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
#include "../config.hpp"
#include "../runtime.hpp"
#include "../log.hpp"
#include "../primitives.hpp"
#include "../mq_system/abc.hpp"

using namespace fmt;
using namespace std;
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
  // Fires when Results for given sample have been collected.
  boost::signals2::signal<void (Results)> sample_result;
  // Fires when Sampler is initiating sampling for given SampleDesc.
  boost::signals2::signal<void (SampleDesc)> sampling;
};

class sampler_abc {
public:
  Observable observable;
  virtual Results run(int n) = 0;
};

// Sampler was made into template so we can use it with fake worker in tests.
// It didn't work out. I don't remember why. Anyway, I'm keeping it for now.
template<class W = Worker>
class Sampler : public sampler_abc {
  Config& config;
  mqs::abc::mq& mq;
  logger& log;
  function<void(milliseconds)> sleep_for;

public:
  Observable observable;

  Sampler(Config&, mqs::abc::mq& mq, logger& log);
  Sampler(
    Config&
  , mqs::abc::mq&
  , logger&
  , function<void(milliseconds)>
  );
  Results run(int n);
};

template class Sampler<Worker>;

#endif
