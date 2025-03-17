#include "./algorithm.hpp"

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
