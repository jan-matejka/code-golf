#include <gtest/gtest.h>

#include "primitives.hpp"

namespace {
TEST(Results, MPS1) {
  auto rs = Results();
  rs.Add(WorkerResult(1, 10, chrono::duration<double>(1)));
  rs.Add(WorkerResult(1, 10, chrono::duration<double>(1)));
  ASSERT_EQ(rs.MessagesPerSecond, float(20));
}


TEST(Results, MPS2) {
  auto rs = Results();
  rs.Add(WorkerResult(1, 10, chrono::duration<double>(1)));
  rs.Add(WorkerResult(1, 10, chrono::duration<double>(1)));
  rs.Add(WorkerResult(1, 10, chrono::duration<double>(1)));
  rs.Add(WorkerResult(1, 10, chrono::duration<double>(1)));
  ASSERT_EQ(rs.MessagesPerSecond, float(40));
}

TEST(Results, MPS3) {
  auto rs = Results();
  rs.Add(WorkerResult(1, 10, chrono::duration<double>(1)));
  rs.Add(WorkerResult(1, 20, chrono::duration<double>(2)));
  ASSERT_EQ(rs.MessagesPerSecond, float(20));
}
}
