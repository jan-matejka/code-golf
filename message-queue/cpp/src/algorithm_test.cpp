#include <gtest/gtest.h>

#include <optional>
#include <list>

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
}
