#ifndef MT_SYSTEM_THREAD_HPP
#define MT_SYSTEM_THREAD_HPP

#include <barrier>
#include <cmath>
#include <iostream>
#include <pqxx/pqxx>
#include <thread>
#include <mutex>
#include <chrono>
#include <queue>
#include <ranges>
#include <syncstream>
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

class Worker {
  int worker_id;
  bool& exit;
  shared_ptr<queue<optional<WorkerResult>>>& result;
  barrier<>& barr;
  bool barriers_passed = false;
  mutex &mut;
  connection conn;

  void insert(int i);
  WorkerResult sample();
  void push(optional<WorkerResult> wr);

public:
  Worker(
    int worker_id,
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
        WVERBOSE(worker_id, "pushing " << wr.MessagesTotal << " into " << result.get());
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

class Sampler {
  Instance& app;

public:
  Observable observable;

  Sampler(Instance &app);
  optional<Results> run(int n);
};

#endif
