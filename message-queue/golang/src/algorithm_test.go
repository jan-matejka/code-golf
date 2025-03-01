package jmcgmqp

import (
	"fmt"
	"iter"
	"reflect"
	"slices"
	"testing"
)

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

func TestFindMaximum(t *testing.T) {
	type TC struct {
		max  func(sampler) *core.Results
		name string
	}

	test := func(tc TC) func(t *testing.T) {
		return func(t *testing.T) {
			var results []*core.Results
			results = append(results, core.NewResults())
			results = append(results, core.NewResults())
			results = append(results, core.NewResults())
			results = append(results, core.NewResults())
			results = append(results, core.NewResults())
			results = append(results, core.NewResults())
			results[0].Add(core.NewWorkerResult(1, 10, 1_000_000_000)) // 1
			results[1].Add(core.NewWorkerResult(1, 20, 1_000_000_000)) // 2
			results[2].Add(core.NewWorkerResult(1, 30, 1_000_000_000)) // 4
			results[3].Add(core.NewWorkerResult(1, 29, 1_000_000_000)) // 8
			results[4].Add(core.NewWorkerResult(1, 35, 1_000_000_000)) // 5
			results[5].Add(core.NewWorkerResult(1, 34, 1_000_000_000)) // 6

			var n_workers []int
			var i int = -1

			sample := func(workers int) *core.Results {
				n_workers = append(n_workers, workers)
				i += 1
				if i >= len(results) {
					return nil
				} else {
					return results[i]
				}
			}

			var r *core.Results
			r = tc.max(sample)
			if r.MessagesTotal != 35 {
				t.Fatalf("max(sample).MessagesTotal=%v, want 35", r)
			}

			expect := []int{1, 2, 4, 8, 5, 6}
			if !reflect.DeepEqual(n_workers, expect) {
				t.Fatalf("Expected %v, got %v", expect, n_workers)
			}
		}
	}

	for _, tc := range []TC{
		TC{FindMaximum, "FindMaximum"},
		TC{FindMaximum2, "FindMaximum2"},
	} {
		t.Run(fmt.Sprintf("%v", tc), test(tc))
	}
}

func TestNIterator(t *testing.T) {
	type TC struct {
		BreakAt int
		Expect  []int
		it      *NIterator
	}

	test := func(tc TC) func(t *testing.T) {
		// wraps the given NIterator with calling Step() on it at given tc.BreakAt
		wrap_step := func(it *NIterator) iter.Seq[int] {
			return func(yield func(int) bool) {
				for n := range it.Iter() {
					if !yield(n) {
						return
					}
					if n == tc.BreakAt {
						it.Step()
					}
				}
			}
		}

		// stops given iterator if max is reached
		wrap_max := func(it iter.Seq[int], max int) iter.Seq[int] {
			return func(yield func(int) bool) {
				i := 0
				for n := range it {
					if !yield(n) || i >= max {
						return
					}
					i++
				}
			}
		}

		// actual test function
		return func(t *testing.T) {
			rs := slices.Collect(wrap_max(wrap_step(tc.it), len(tc.Expect)+1))
			if !reflect.DeepEqual(rs, tc.Expect) {
				t.Fatalf("rs=%v != expect=%v", rs, tc.Expect)
			}
		}
	}

	for _, tc := range []TC{
		TC{1, []int{1}, &NIterator{}},
		TC{2, []int{1, 2}, &NIterator{}},
		TC{4, []int{1, 2, 4, 3}, &NIterator{}},
		TC{8, []int{1, 2, 4, 8, 5, 6, 7}, &NIterator{}},
	} {
		t.Run(fmt.Sprintf("%v", tc), test(tc))
	}
}

func Test_betweenPowers(t *testing.T) {
	type TC struct {
		N      int
		Expect []int
	}

	test := func(tc TC) func(*testing.T) {
		return func(t *testing.T) {
			xs := slices.Collect(betweenPowers(tc.N))

			if !reflect.DeepEqual(xs, tc.Expect) {
				t.Fatalf("xs=%#v != expect=%#v", xs, tc.Expect)
			}
		}
	}

	for _, tc := range []TC{
		TC{1, nil},
		TC{2, []int{3}},
		TC{4, []int{5, 6, 7}},
	} {
		t.Run(fmt.Sprintf("%v", tc), test(tc))
	}
}
