#ifndef ALGORITHM_HPP
#define ALGORITHM_HPP

#include <optional>
#include <functional>
#include <cmath>
#include <ranges>

#include "./primitives.hpp"

using namespace std;

optional<Results> FindMaximum(
  function<optional<Results>(int)> sample,
  int starting_power=0
);

#endif
