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

#if O_VERBOSE == 1
#define VERBOSE(x) osyncstream(cout) << x << endl
#else
#define VERBOSE(x)
#endif

#define INFO(x) osyncstream(cout) << x << endl
#define ERR(x) osyncstream(cerr) << x << endl
#define WVERBOSE(id, x) VERBOSE("Worker " << id << ": " << x)
#define WERR(id, x) ERR("Worker " << id << ": " << x)

#define igetenv(x, def) getenv(x) ? stoi(getenv(x)) : def

using namespace std;
using namespace pqxx;

void insert(connection &C, int i) {
  work W(C);
  W.exec("insert into queue (data) values (" + W.quote(i) + ")");
  W.commit();
}

void worker(
int worker_id,
shared_ptr<queue<bool>>& exit,
shared_ptr<queue<int>>& result
) {
  WVERBOSE(worker_id, "starting");
  WVERBOSE(worker_id, "got result q " << result.get());
  try {
    connection C("postgres://mq@localhost/mq");
    if (!C.is_open()) {
      WERR(worker_id, "Can't open database");
      return;
    }

    int i=0;
    while(exit->empty()) {
      insert(C, i++);
    }

    WVERBOSE(worker_id, "pushing " << i << " into " << result.get());
    result->push(i);
    C.disconnect();
  } catch (const std::exception &e) {
    cerr << e.what() << std::endl;
  }
}

int sample_workers(int n) {
  INFO("Starting " << n << " workers");
  auto exit = make_shared<queue<bool>>();
  std::vector<shared_ptr<queue<int>>> result_qs;
  std::vector<shared_ptr<jthread>> threads;

  for (int worker_id : ranges::views::iota(1, n+1)) {
    auto result = make_shared<queue<int>>();
    result_qs.push_back(result);

    shared_ptr<jthread> w = make_shared<jthread>(
      bind(worker, worker_id, ref(exit), result)
    );
    threads.push_back(w);
  }

  int dur = igetenv("WORK_DURATION", 3);
  VERBOSE("Duration: " << dur << "s");
  INFO("Waiting");
  for(auto i : ranges::views::iota(0, dur)) {
    INFO((dur-i) << "s");
    this_thread::sleep_for(chrono::seconds(1));
  }

  VERBOSE("pushing exit messages");
  exit->push(true);

  VERBOSE("joining threads");
  for(auto& t : threads) {
    t->join();
  }

  VERBOSE("collecting results");
  int total=0;
  for(shared_ptr<queue<int>> q : result_qs) {
    while(q->empty()) {
      VERBOSE("empty queue " << q.get());
      this_thread::sleep_for(chrono::seconds(1));
    }
    int txs = q->front();
    total += txs;
    INFO(txs);
  }

  INFO("Total: " << total);
  cout << endl;
  return total;
}

int main(void) {
  int last=0;
  auto start = igetenv("START_POWER", 0);
  for(auto i : ranges::views::iota(start)) {
    auto r = sample_workers(pow(2, i));
    if (r <= last)
      break;

    last = r;
  }

  return 0;
}
