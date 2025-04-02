#include <gtest/gtest.h>

#include <optional>
#include <list>

// #include <fmt/format.h>

#include "algorithm.hpp"
#include "primitives.hpp"

using namespace std;

class Sampler {
public:
  list<int> calls;
  optional<Results> operator()(int n) {
    calls.push_back(n);
    auto rs = Results();
    switch(n) {
      case 1:
        rs.Add(WorkerResult(1, 1, chrono::duration<double>(1)));
        break;
      case 2:
        rs.Add(WorkerResult(1, 2, chrono::duration<double>(1)));
        break;
      case 4:
        rs.Add(WorkerResult(1, 3, chrono::duration<double>(1)));
        break;
      case 8:
        rs.Add(WorkerResult(1, 2, chrono::duration<double>(1)));
        break;
      case 5:
        rs.Add(WorkerResult(1, 4, chrono::duration<double>(1)));
        break;
      case 6:
        rs.Add(WorkerResult(1, 3, chrono::duration<double>(1)));
        break;
      default:
        stringstream ss;
        ss << "Unexpected N=" << n;
        throw runtime_error(ss.str());
    }

    return rs;
  }
};

namespace {
TEST(FindMaximumTest, Test) {
  Sampler s;
  auto rs = FindMaximum(ref(s));
  ASSERT_EQ(rs.value().MessagesPerSecond, 4);
  list<int> expected = {1, 2, 4, 8, 5, 6};
  ASSERT_EQ(expected, s.calls);
}

TEST(Powers, GivesPowers) {
  auto it = powers();
  ASSERT_EQ(*it, 1);

  it++;
  ASSERT_EQ(*it, 2);

  it++;
  ASSERT_EQ(*it, 4);

  it++;
  ASSERT_EQ(*it, 8);
}

TEST(successor, gives_successors) {
  auto it = successor();
  ASSERT_EQ(*it, 0);

  it++;
  ASSERT_EQ(*it, 1);

  it++;
  ASSERT_EQ(*it, 2);

  it++;
  ASSERT_EQ(*it, 3);

  it = successor(10);
  ASSERT_EQ(*it, 10);

  it++;
  ASSERT_EQ(*it, 11);
}

TEST(sampler_iterator, works) {
  auto it = sample_iterator();
  ASSERT_EQ(*it, 1);

  it++;
  ASSERT_EQ(*it, 2);

  it++;
  ASSERT_EQ(*it, 4);

  it.next_iter();
  ASSERT_EQ(*it, 2);

  it++;
  ASSERT_EQ(*it, 3);

  it.next_iter();

  EXPECT_THROW(*it, runtime_error);

  it++;
  EXPECT_THROW(it++, runtime_error);
}

TEST(sampler_iterator, in_for) {
  auto it = sample_iterator();
  int expect_ns[] = {1, 2, 4, 8, 5, 6};
  for(int cnt=0; it.is_valid(); it++, cnt++) {
    auto n = *it;
    ASSERT_EQ(n, expect_ns[cnt]);
    if (cnt == 3)
      it.next_iter();

    if (cnt == 5)
      it.next_iter();

    if (cnt > 5)
      FAIL() << "We shouldn't get there";
  }
}
}
