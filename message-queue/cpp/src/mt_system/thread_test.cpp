#include <gtest/gtest.h>

#include "thread.hpp"
#include "../mq_system/abc.hpp"

namespace {

class sender : public mqs::abc::sender {
  void send(int i) {}
};

class mq : public mqs::abc::mq {
  unique_ptr<mqs::abc::sender> connect() {
    return make_unique<sender>();
  }
};

TEST(Sampler, sample_result_signal) {
  auto cg = Config();
  auto mqs = mq();
  auto log = logger::null();
  auto s = Sampler<Worker>(cg, mqs, log, [](milliseconds _){});
  bool obs_called = false;
  auto observer = [&obs_called](Results rs) {
    obs_called = true;
  };
  s.observable.sample_result.connect(observer);
  s.run(2);
  ASSERT_EQ(obs_called, true);
}
}
