#include <fmt/format.h>
#include <functional>

#include "../algorithm.hpp"
#include "../log.hpp"
#include "../instance.hpp"
#include "../mq_system/postgres.hpp"
#include "../mt_system/thread.hpp"
#include "../telemetry/prometheus.hpp"

using namespace fmt;
using namespace std;
using namespace pqxx;
using namespace std::placeholders;

int _main(void) {
  auto app = Instance();
  INFO(format("Config: {}", app.config.str()));

  if (app.config.test_prometheus) {
    PushTestMetric(app);
    return 0;
  }

  auto log = logger();
  auto pgmq = mqs::postgres::mq(app.config);
  auto sampler = Sampler<>(ref(app), pgmq, log);
  auto max = FindMaximum(
    bind(&Sampler<>::run, &sampler, _1),
    app.config.power
  );
  if (max.has_value()) {
    INFO("Found maximum:");
    max.value().Print(log);
  }else{
    THROW("Maximum is nullopt", nullptr);
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
