#ifndef ALGORITHM_CPP
#define ALGORITHM_CPP

#include "./algorithm.hpp"

using namespace std::placeholders;

inline void powers::postinc(int& n) {
  n = n << 1;
};

inline void successor::postinc(int& n) {
  n++;
};

int_iter::int_iter(int n, function<void(int&)> postinc)
: n(n), postinc(postinc) {};

powers::powers(int p) : int_iter(1 << p, ref(powers::postinc)) {};
successor::successor(int first) : int_iter(first, ref(successor::postinc)) {};

sample_iterator::sample_iterator(int starting_power)
: it(powers(starting_power)) {
  // fmt::print("{}: constructed", (string)*this);
};

void sample_iterator::on_result(Results rs) {
  // fmt::print("{}: on_result: {}\n", (string)*this, rs);

  if (!prev || rs > prev.value()) {
    prev = rs;
    return;
  }

  // rs <= prev
  next_iter();
};

void sample_iterator::check_state(int _invalid) const {
  if (is_valid(_invalid))
    return;

  auto s = format("{}: no longer valid", (string)*this);
  throw runtime_error(s);
};

void sample_iterator::next_iter() {
  state++;

  if (state > 1)
    return;

  int n = *it;
  n = n >> 1;
  it = successor(n);
};

void sample_iterator::connect(sampler_abc& s) {
  auto sub = bind(&sample_iterator::on_result, this, _1);
  s.observable.sample_result.connect(sub);
};

Results max_element(
  sampler_abc& sampler,
  int starting_power
) {
  auto it = sample_iterator(starting_power);
  it.connect(sampler);
  optional<Results> max = nullopt;

  for(;it.is_valid(); it++) {
    auto rs = sampler.run(*it);
    if (!max || rs > max.value()) {
      max = rs;
      continue;
    }
  }

  return max.value();
};

#endif
