#include <iostream>
#include <pqxx/pqxx>
#include <thread>
#include <chrono>
#include <queue>
#include <ranges>

using namespace std;
using namespace pqxx;

void insert(connection &C, int i) {
  work W(C);
  W.exec("insert into queue (data) values (" + W.quote(i) + ")");
  W.commit();
}

void worker(shared_ptr<queue<bool>> &q) {
  try {
    connection C("postgres://mq@localhost/mq");
    if (!C.is_open()) {
      cerr << "Can't open database" << endl;
      return;
    }

    int i;
    for (i=0;q->empty();i++) {
      insert(C, i);
    }

    cout << i << "\n";

    C.disconnect();
  } catch (const std::exception &e) {
    cerr << e.what() << std::endl;
  }
}

void sample_workers(int n) {
  std::vector<shared_ptr<queue<bool>>> queues;
  std::vector<shared_ptr<jthread>> threads;

  // for (auto _ : ranges::views::iota(0)) {
  for(int i=0; i<n; i++)  {
    auto q = make_shared<queue<bool>>();
    queues.push_back(q);
    shared_ptr<jthread> w = make_shared<jthread>(worker, ref(q));
    threads.push_back(w);
  }

  for(auto i : ranges::views::iota(0, 3)) {
    this_thread::sleep_for(chrono::seconds(1));
    cout << i << "\n";
  }

  for(auto& q : queues) {
    q->push(true);
  }
}

int main(void) {
  sample_workers(2);

  return 0;
}
