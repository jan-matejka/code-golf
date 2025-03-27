#ifndef ALGORITHM_HPP
#define ALGORITHM_HPP

#include <optional>
#include <functional>
#include <cmath>
#include <ranges>

#include "./primitives.hpp"

using namespace std;

class powers {
  int n;
public:
  powers(int p=0) : n(1 << p) {};

  int operator*() const {
    return n;
  }

  int* operator->() {
    return &n;
  }

  // Prefix increment
  powers& operator++() {
    n = n << 1;
    return *this;
  }

  // Postfix increment
  powers operator++(int) {
    powers tmp = *this;
    ++(*this);
    return tmp;
  }

  friend bool operator== (const powers& a, const powers& b) {
    return a.n == b.n;
  };
  friend bool operator!= (const powers& a, const powers& b) {
    return a.n != b.n;
  };
};

optional<Results> FindMaximum(
  function<optional<Results>(int)> sample,
  int starting_power=0
);

#endif
