#include <gtest/gtest.h>

#include <algorithm>

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

TEST(Results, AreComparable) {
  auto rs1 = Results();
  auto rs2 = Results();
  rs1.Add(WorkerResult(1, 10, chrono::duration<double>(1)));
  rs2.Add(WorkerResult(1, 20, chrono::duration<double>(1)));
  ASSERT_LT(rs1, rs2);

  Results _max = max(rs1, rs2);
  ASSERT_EQ(_max, rs2);
}
}
