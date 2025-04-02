#ifndef ALGORITHM_HPP
#define ALGORITHM_HPP

#include <optional>
#include <functional>
#include <cmath>
#include <ranges>

#include "./primitives.hpp"

using namespace std;

class int_iter {
protected:
  int n;
  function<void(int&)> postinc;
  int_iter(int, function<void(int&)>);
public:
  int operator*() const {
    return n;
  }

  int* operator->() {
    return &n;
  }

  int_iter& operator++() {
    postinc(n);
    return *this;
  }

  int_iter operator++(int) {
    int_iter tmp = *this;
    ++(*this);
    return tmp;
  }

  friend bool operator== (const int_iter& a, const int_iter& b) {
    return a.n == b.n;
  };

  friend bool operator!= (const int_iter& a, const int_iter& b) {
    return a.n != b.n;
  };
};

class powers : public int_iter {
public:
  powers(int p=0);
  inline static void postinc(int& n);
};

class successor : public int_iter {
public:
  successor(int first=0);
  inline static void postinc(int& n);
};

optional<Results> FindMaximum(
  function<optional<Results>(int)> sample,
  int starting_power=0
);

#endif
