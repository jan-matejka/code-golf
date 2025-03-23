#ifndef TELEMETRY_ABC_CPP
#define TELEMETRY_ABC_CPP

#include "abc.hpp"

using namespace std::placeholders;

namespace telemetry::abc {

void pusher_abc::observe(const Runtime& rt, Sampler<Worker>& s) {
  auto obs = observer_t(this, rt);
  observer.emplace(obs);
  observer_t* o = &(observer.value());

  s.observable.sampling.connect(bind(&observer_t::sampling, o, _1));
  s.observable.sample_result.connect(bind(&observer_t::results, o, _1));
}

observer_t::observer_t(pusher_abc* p, const Runtime& rt)
: pusher(p), runtime(rt) {}

void observer_t::sampling(SampleDesc sdesc_p) {
  sdesc.emplace(sdesc_p);
}

void observer_t::results(Results rs) {
  pusher->Push(runtime, sdesc.value(), rs);
}

}

#endif
