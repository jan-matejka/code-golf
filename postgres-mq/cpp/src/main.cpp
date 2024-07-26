#include <barrier>
#include <cmath>
#include <iostream>
#include <pqxx/pqxx>
#include <thread>
#include <chrono>
#include <queue>
#include <ranges>
#include <syncstream>
#include <functional>
#include <memory>
#include <optional>

#define VERBOSE(x) if(igetenv("VERBOSE", 0)) osyncstream(cout) << x << endl
#define INFO(x) osyncstream(cout) << x << endl
#define ERR(x) osyncstream(cerr) << x << endl
#define WVERBOSE(id, x) VERBOSE("Worker " << id << ": " << x)
#define WERR(id, x) ERR("Worker " << id << ": " << x)

#define igetenv(x, def) getenv(x) ? stoi(getenv(x)) : def

using namespace std;
using namespace pqxx;


class Worker {
  int worker_id;
  bool& exit;
  shared_ptr<queue<optional<int>>>& result;
  barrier<>& barr;
  int barrier_passed = 0;
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

    barr.arrive_and_wait();
    barrier_passed++;

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
    barrier<>& barr
  ) :
    worker_id(worker_id),
    exit(exit),
    result(result),
    barr(barr),
    conn(connection("postgres://mq@localhost/mq")
  ) {
    if (!conn.is_open()) {
      stringstream ss;
      ss << "Worker " << worker_id << " can't open database";
      throw new runtime_error(ss.str());
    }
  }

  ~Worker() {
    try {
      conn.disconnect();
    }catch(...) {}

    for(int i=1-barrier_passed;i>0;i--)
    {
      WVERBOSE(worker_id, "awaiting barrier " << i);
      barr.arrive_and_wait();
    }
  }

  void operator()() {
    try {
      try {
        int i = sample();
        WVERBOSE(worker_id, "pushing " << i << " into " << result.get());
        result->push(i);
      } catch (const std::exception &e) {
        WERR(worker_id, e.what());
        throw;
      }
    } catch (...) {
      WVERBOSE(worker_id, "pushing nullopt");
      result->push(nullopt);
    }
  }
};

optional<int> sample_workers(int n) {
  INFO("Starting " << n << " workers");
  bool exit = false;
  auto results = make_shared<queue<optional<int>>>();
  std::vector<shared_ptr<jthread>> threads;
  vector<shared_ptr<Worker>> workers;
  barrier b(n+1);

  for (int worker_id : ranges::views::iota(1, n+1)) {
    auto worker = make_shared<Worker>(worker_id, ref(exit), ref(results), ref(b));
    workers.push_back(worker);
    shared_ptr<jthread> w = make_shared<jthread>(&Worker::operator(), worker);
    threads.push_back(w);
  }

  int dur = igetenv("WORK_DURATION", 3);
  VERBOSE("Duration: " << dur << "s");

  // this barrier syncs all threads on ready to send out messages
  b.arrive_and_wait();
  auto start = chrono::steady_clock::now();

  INFO("Waiting");
  for(auto i : ranges::views::iota(0, dur)) {
    INFO((dur-i) << "s");
    this_thread::sleep_for(chrono::seconds(1));
  }

  VERBOSE("stopping workers");
  exit = true;
  auto end = chrono::steady_clock::now();

  VERBOSE("joining threads");
  for(auto& t : threads) {
    t->join();
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
  INFO("Total txs: " << txps);
  cout << endl;
  return total;
}

int _main(void) {
  int last=0;
  auto start = igetenv("START_POWER", 0);
  int i;
  for(i=start;;i++) {
    int n = pow(2, i);
    auto r = sample_workers(n);
    if (r.has_value()) {
      if (r <= last)
        break;

      last = r.value();
    }else{
      ERR("failed to sample " << n << " workers");
      return 1;
    }
  }

  i = pow(2, i-1) + 1;
  for(auto n : ranges::views::iota(i)) {
    auto r = sample_workers(i);
    if (r.has_value()) {
      if (r <= last)
        break;

      last = r.value();
    }else{
      ERR("failed to sample 2^" << n << " workers");
      return 1;
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
