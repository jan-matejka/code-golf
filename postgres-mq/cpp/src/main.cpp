#include <iostream>
#include <pqxx/pqxx>
#include <thread>
#include <chrono>
#include <queue>

using namespace std;
using namespace pqxx;

void insert(connection &C, int i) {
  work W(C);
  W.exec("insert into queue (data) values (" + W.quote(i) + ")");
  W.commit();
}

void worker(queue<bool> &q) {
  try {
    connection C("postgres://mq@localhost/mq");
    if (!C.is_open()) {
      cerr << "Can't open database" << endl;
      return;
    }

    int i;
    for (i=0;q.empty();i++) {
      insert(C, i);
    }

    cout << i << "\n";

    C.disconnect();
  } catch (const std::exception &e) {
    cerr << e.what() << std::endl;
  }
}

int main(void) {
  queue<bool> q;
  jthread w(worker, ref(q));
  this_thread::sleep_for(chrono::seconds(3));
  q.push(true);

  return 0;
}
