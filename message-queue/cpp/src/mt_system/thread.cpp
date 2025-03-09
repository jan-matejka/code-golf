#ifndef MT_SYSTEM_THREAD_CPP
#define MT_SYSTEM_THREAD_CPP

#include "./thread.hpp"

void Worker::insert(int i) {
  work tx(conn);
  tx.exec("insert into queue (data) values (" + tx.quote(i) + ")");
  tx.commit();
}

WorkerResult Worker::sample() {
  WVERBOSE(worker_id, "starting");
  WVERBOSE(worker_id, "got result q " << result.get());
  WVERBOSE(worker_id, "ready for work");

  barriers_passed = true;
  barr.arrive_and_wait();

  auto start = chrono::steady_clock::now();

  int i=0;
  while(!exit) {
    insert(i++);
  }

  auto end = chrono::steady_clock::now();
  auto wr = WorkerResult(worker_id, i, end-start);
  return wr;
}

Worker::Worker(
  int worker_id,
  bool& exit,
  shared_ptr<queue<optional<WorkerResult>>>& result,
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

Worker::~Worker() {
  try {
    conn.disconnect();
  }catch(...) {}

  if (!barriers_passed)
    barr.arrive_and_drop();
}

void Worker::push(optional<WorkerResult> wr) {
  try{
    WVERBOSE(worker_id, "awaiting lock");
    mut.lock();
    result->push(wr);
    mut.unlock();
  }catch(...) {
    mut.unlock();
    throw;
  }
}

void SetAndPushMetrics(
  Instance &app,
  const Results &rs,
  const SampleDesc &sdesc
) {
  for(const auto& wr : rs.Workers) {
    app.prometheus.messages_total
      .Add(mk_labels(app, wr, sdesc))
      .Set(wr.MessagesTotal);

    app.prometheus.messages_per_second
      .Add(mk_labels(app, wr, sdesc))
      .Set(wr.MessagesPerSecond);

    app.prometheus.duration_seconds
      .Add(mk_labels(app, wr, sdesc))
      .Set(wr.DurationSeconds);
  }

  app.prometheus.Push();
  app.pg.Push(app.runtime, sdesc, rs);
}

template<class W>
Sampler<W>::Sampler(Instance &app) : app(app) {}

template<class W>
optional<Results> Sampler<W>::run(int n) {
  INFO("Starting " << n << " workers");
  bool exit = false;
  auto results = make_shared<queue<optional<WorkerResult>>>();
  vector<shared_ptr<W>> workers;
  barrier b(n+1);
  auto c = app.config;

  {
    mutex mut;
    std::vector<shared_ptr<jthread>> threads;
    for (int worker_id : ranges::views::iota(1, n+1)) {
      shared_ptr<W> worker;
      try {
        worker = make_shared<W>(
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
      shared_ptr<jthread> w = make_shared<jthread>(&W::operator(), worker);
      threads.push_back(w);
    }

    // this barrier syncs all threads on ready to send out messages
    b.arrive_and_wait();

    INFO("Waiting");
    for(auto i : ranges::views::iota(0, c.duration)) {
      INFO((c.duration-i) << "s");
      this_thread::sleep_for(chrono::seconds(1));
    }

    exit = true;
  }

  VERBOSE("collecting results");
  Results rs;
  for(int i : ranges::views::iota(0, n)) {
    for(int j = 0; results->empty(); j++) {
      if (j % 1000 == 0)
        INFO("awaiting results from " << results.get() << ": " << n-i << " left");
      this_thread::sleep_for(chrono::milliseconds(1));
    }
    auto r = results->front();
    if (r.has_value()) {
      rs.Add(r.value());
      results->pop();
    }else{
      return nullopt;
    }
  }

  rs.Print();
  cout << endl;

  auto sdesc = SampleDesc(n, "threading", "postgres");
  observable.sample_result(rs);
  SetAndPushMetrics(app, rs, sdesc);

  return rs;
}

#endif
