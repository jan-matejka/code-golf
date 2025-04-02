#ifndef ALGORITHM_CPP
#define ALGORITHM_CPP

#include "./algorithm.hpp"

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

optional<Results> FindMaximum(
  function<optional<Results>(int)> sample,
  int starting_power
) {
  optional<Results> prev = nullopt;
  int i = starting_power;
  for(;;i++) {
    int n = pow(2, i);
    auto opt = sample(n);
    if (opt.has_value()) {
      auto rs = opt.value();
      if (prev.has_value() && rs.MessagesPerSecond <= prev.value().MessagesPerSecond)
        break;

      prev = rs;
    }else{
      THROW("failed to sample {} workers", n);
    }
  }

  i = pow(2, i-1) + 1;
  for(auto n : ranges::views::iota(i)) {
    auto opt = sample(n);
    if (opt.has_value()) {
      auto rs = opt.value();
      if (prev.has_value() and rs.MessagesPerSecond <= prev.value().MessagesPerSecond)
        break;

      prev = rs;
    }else{
      THROW("failed to sample {} workers", n);
    }
  }

  return prev;
}

#endif
