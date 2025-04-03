#ifndef ALGORITHM_HPP
#define ALGORITHM_HPP

#include <optional>
#include <functional>
#include <cmath>
#include <ranges>

#include <fmt/format.h>

#include "./primitives.hpp"
#include "./mt_system/thread.hpp"

using namespace std;

class int_iter {
public:
  // n needs to be public so sample_iterator-> can return its address.
  int n;
protected:
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


class sample_iterator {
  int_iter it;
  optional<Results> prev = nullopt;
  int state = 0;
public:
  sample_iterator(int starting_power=0);

  int operator*() const {
    // fmt::print("{}: deref\n", (string)*this);
    check_state();
    return *it;
  }

  int* operator->() {
    check_state();
    return &it.n;
  }

  sample_iterator& operator++() {
    // fmt::print("{}: preinc\n", (string)*this);
    check_state(2);
    if (!is_valid())
      state++;
    it++;
    return *this;
  }

  sample_iterator operator++(int) {
    // fmt::print("{}: postinc\n", (string)*this);
    check_state(2);
    if (!is_valid())
      state++;

    auto tmp = *this;
    ++it;
    return tmp;
  }

  friend bool operator== (const sample_iterator& a, const sample_iterator& b) {
    return a.it.n == b.it.n;
  };

  friend bool operator!= (const sample_iterator& a, const sample_iterator& b) {
    return a.it.n != b.it.n;
  };

  operator string() const {
    auto addr = reinterpret_cast<uintptr_t>(this);
    return fmt::format(
      "<sample_iterator id={} state={} n={}>",
      addr,
      state,
      *it
    );
  };

  void on_result(Results rs);
  void next_iter();
  void check_state(int _invalid=1) const;
  inline bool is_valid(int _invalid=1) const {
    /**
     * Note the intended use case looks like:
     *    for(;it.is_valid();it++) {
     *        if(fail) it.next_iter();
     *    }
     *
     * Therefore the final sequence will be:
     *   1. it.next_iter() // invalidates the iterator
     *   2. it++ // the for's postincrement kicks in
     *   3. it.is_valid() terminates the for loop.
     *
     * For this reason we want to offset the invalid state by 1 when checking
     * during {pre,post}-increments so the first call to it does not throw as
     * that is part of normal operation but subsequent calls do throw.
     * So e.g. when the check for is_valid() is forgotten or otherwise
     * invalidated the caller does not get stuck in an infinite loop.
     */
    return state <= _invalid;
  };
};

/**
 * std::max_element overload for our sampler.
 *
 * We could make an interface to fit into std::max_element() but no matter what
 * we do, it's not gonna fulfil the semantics of forward_iterator.
 * So instead we gonna overload the max_element definition.
 */
Results max_element(sampler_abc& sampler, int starting_power=0);

#endif
