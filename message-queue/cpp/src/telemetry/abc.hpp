#ifndef TELEMETRY_ABC_HPP
#define TELEMETRY_ABC_HPP

#include <optional>

#include "../config.hpp"
#include "../runtime.hpp"
#include "../primitives.hpp"
#include "../mt_system/thread.hpp"

using namespace std;

namespace telemetry::abc {

class pusher_abc;

class observer_t {
  /** pusher always outlives observer.
   */
  pusher_abc* pusher;
  const Runtime runtime;
  optional<SampleDesc> sdesc;

public:
  observer_t(pusher_abc* p, const Runtime rt);
  /**
   * Callback.
   */
  void sampling(SampleDesc sdesc);
  /**
   * Callback.
   */
  void results(Results rs);
};

class pusher_abc {
  optional<observer_t> observer;
public:
  virtual void Push(const Runtime&, const SampleDesc&, const Results&) = 0;
  void observe(Runtime rt, Sampler<Worker>&);
};

}

#endif
