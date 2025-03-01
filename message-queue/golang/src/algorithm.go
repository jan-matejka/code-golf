package jmcgmqp

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

type sampler = func(int) *core.Results

// Run `sample` with increasing powers until its result decreases.
// Then run `sample` with increments from last input that returned non-decreasing value.
// Returns the last non-decreasing result or nil if no result was obtained.
func FindMaximum(sample sampler) *core.Results {
	return findMaximum2(sample, 0)
}

func findMaximum2(sample sampler, starting_power int) *core.Results {
	var prev *core.Results
	var workers int
	var i int
	for ; ; i++ {
		workers = 1 << i
		r := sample(workers)
		if r == nil {
			panic("Unexpected nil from sample")
		}
		if prev != nil && prev.MessagesPerSecond >= r.MessagesPerSecond {
			i--
			break
		} else {
			prev = r
		}
	}

	for workers = 1<<i + 1; workers < 1<<(i+1); workers++ {
		r := sample(workers)
		if r == nil {
			panic("Unexpected nil from sample")
		}
		if prev != nil && prev.MessagesPerSecond >= r.MessagesPerSecond {
			break
		} else {
			prev = r
		}
	}

	return prev
}
