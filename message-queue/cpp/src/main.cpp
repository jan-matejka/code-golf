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

#include <boost/algorithm/string.hpp>
#include "./config.cpp"
#include "./runtime.cpp"
#include "./prometheus.cpp"
#include "./log.cpp"

using namespace std;
using namespace pqxx;

class Worker {
  int worker_id;
  bool& exit;
  shared_ptr<queue<optional<int>>>& result;
  barrier<>& barr;
  bool barriers_passed = false;
  mutex &mut;
  connection conn;

  void insert(int i) {
    work tx(conn);
    tx.exec("insert into queue (data) values (" + tx.quote(i) + ")");
    tx.commit();
  }

  int sample() {
    WVERBOSE(worker_id, "starting");
    WVERBOSE(worker_id, "got result q " << result.get());
    WVERBOSE(worker_id, "ready for work");

    barriers_passed = true;
    barr.arrive_and_wait();

    int i=0;
    while(!exit) {
      insert(i++);
    }

    return i;
  }

public:
  Worker(
    int worker_id,
    bool& exit,
    shared_ptr<queue<optional<int>>>& result,
    barrier<>& barr,
    mutex& mut
  ) try :
    worker_id(worker_id),
    exit(exit),
    result(result),
    barr(barr),
    mut(mut),
    conn(connection("postgres://mq@localhost/mq"))
  {
  }catch(...) {
    barr.arrive_and_drop();
    throw;
  }

  ~Worker() {
    try {
      conn.disconnect();
    }catch(...) {}

    if (!barriers_passed)
      barr.arrive_and_drop();
  }

  void operator()() {
    try {
      try {
        int i = sample();
        WVERBOSE(worker_id, "pushing " << i << " into " << result.get());
        push(i);
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

  void push(optional<int> i) {
    try{
      WVERBOSE(worker_id, "awaiting lock");
      mut.lock();
      result->push(i);
      mut.unlock();
    }catch(...) {
      mut.unlock();
      throw;
    }
  }
};

optional<int> sample_workers(Config c, int n) {
  INFO("Starting " << n << " workers");
  bool exit = false;
  auto results = make_shared<queue<optional<int>>>();
  vector<shared_ptr<Worker>> workers;
  barrier b(n+1);
  chrono::time_point<chrono::steady_clock> start, end;

  {
    mutex mut;
    std::vector<shared_ptr<jthread>> threads;
    for (int worker_id : ranges::views::iota(1, n+1)) {
      shared_ptr<Worker> worker;
      try {
        worker = make_shared<Worker>(
          worker_id, ref(exit), ref(results), ref(b), ref(mut)
        );
      }catch(...) {
        exit = true;
        // discard the arrival token, we are unblocking all the threads
        // This is somewhat complicated. Note:
        // - The Worker unblocks its own barrier.
        // - We index from 1 to n.
        // - And we also have the main thread.
        // Therefore, we arrive for::
        //   n - worker_id ; basic calculation
        //   -1 ; standard for zero based index
        //   +1 ; but we index from 1
        //   -1 : for the failed worker
        //   +1 : the Worker already arrived in its constructor exception handler
        //   +1 : for the main thread
        //  which reduces to the calculation below.
        auto _ = b.arrive(n-worker_id+1);
        throw;
      }
      workers.push_back(worker);
      shared_ptr<jthread> w = make_shared<jthread>(&Worker::operator(), worker);
      threads.push_back(w);
    }

    // this barrier syncs all threads on ready to send out messages
    b.arrive_and_wait();
    start = chrono::steady_clock::now();

    INFO("Waiting");
    for(auto i : ranges::views::iota(0, c.duration)) {
      INFO((c.duration-i) << "s");
      this_thread::sleep_for(chrono::seconds(1));
    }

    exit = true;
    end = chrono::steady_clock::now();
  }

  VERBOSE("collecting results");
  int total=0;
  for(int i : ranges::views::iota(0, n)) {
    for(int j = 0; results->empty(); j++) {
      if (j % 1000 == 0)
        INFO("awaiting results from " << results.get() << ": " << n-i << " left");
      this_thread::sleep_for(chrono::milliseconds(1));
    }
    auto r = results->front();
    if (r.has_value()) {
      total += r.value();
      results->pop();
      INFO(r.value());
    }else{
      return nullopt;
    }
  }

  INFO("Total: " << total);
  double secs = chrono::duration<double>(end-start).count();
  float txps = total / secs;
  INFO(secs);
  INFO("Total txps: " << txps);
  cout << endl;
  return total;
}

int _main(void) {
  auto app = Instance();
  INFO("Config: " << app.config.str());

  if (app.config.test_prometheus) {
    PushTestMetric(app.config);
    return 0;
  }
  int last=0;
  int i = app.config.power;
  for(;;i++) {
    int n = pow(2, i);
    auto r = sample_workers(app.config, n);
    if (r.has_value()) {
      if (r <= last)
        break;

      last = r.value();
    }else{
      THROW("failed to sample " << n << " workers");
    }
  }

  i = pow(2, i-1) + 1;
  for(auto n : ranges::views::iota(i)) {
    auto r = sample_workers(app.config, i);
    if (r.has_value()) {
      if (r <= last)
        break;

      last = r.value();
    }else{
      THROW("failed to sample " << n << " workers");
    }
  }

  return 0;
}

int main(void) {
  try {
    _main();
  }catch (const exception& e) {
    ERR(e.what());
    return 1;
  }catch(...) {
    ERR("unknown exception");
    return 1;
  }
}
