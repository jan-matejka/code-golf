package jmcgmqp

import (
	"reflect"
	"testing"
)

func TestFindMaximum(t *testing.T) {
	var results []*Results
	results = append(results, NewResults())
	results = append(results, NewResults())
	results = append(results, NewResults())
	results = append(results, NewResults())
	results = append(results, NewResults())
	results = append(results, NewResults())
	results[0].Add(NewWorkerResult(1, 10, 1_000_000_000)) // 1
	results[1].Add(NewWorkerResult(1, 20, 1_000_000_000)) // 2
	results[2].Add(NewWorkerResult(1, 30, 1_000_000_000)) // 4
	results[3].Add(NewWorkerResult(1, 29, 1_000_000_000)) // 8
	results[4].Add(NewWorkerResult(1, 35, 1_000_000_000)) // 5
	results[5].Add(NewWorkerResult(1, 34, 1_000_000_000)) // 6

	var n_workers []int
	var i int = -1

	sample := func(workers int) *Results {
		n_workers = append(n_workers, workers)
		i += 1
		return results[i]
	}

	var r *Results
	r = FindMaximum(sample)
	if r.MessagesTotal != 35 {
		t.Fatalf("FindMaximum(sample) = %v, want nil", r)
	}

	expect := []int{1, 2, 4, 8, 5, 6}
	if !reflect.DeepEqual(n_workers, expect) {
		t.Fatalf("Expected %v, got %v", expect, n_workers)
	}
}
