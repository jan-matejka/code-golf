package jmcgmqp

import (
	"iter"
	"math"
	"slices"
)

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

type sampler = func(int) *core.Results

// powers() returns Iterator yielding powers of 2
func powers() iter.Seq[int] {
	return func(yield func(int) bool) {
		for i := range math.MaxInt {
			if !yield(1 << i) {
				return
			}
		}
	}
}

// betweenPowers returns Iterator yielding integers in the range (n, n << 1)
func betweenPowers(n int) iter.Seq[int] {
	if n < 2 {
		return func(yield func(int) bool) {
			return
		}
	}

	max := n << 1
	n++

	return func(yield func(int) bool) {
		for ; n < max; n++ {
			if !yield(n) {
				return
			}
		}
	}
}

type NIterator struct {
	step int
}

func (it *NIterator) Step() {
	it.step++
}

func (it *NIterator) Iter() iter.Seq[int] {
	return func(yield func(int) bool) {
		n := 0
		for n = range powers() {
			if it.step > 0 {
				break
			}

			if !yield(n) {
				return
			}
		}

		for n = range betweenPowers(n >> 2) {
			if it.step == 2 || !yield(n) {
				return
			}
		}
	}
}

type SampleIterator struct {
	it *NIterator
}

func NewSampleIterator() *SampleIterator {
	it := new(SampleIterator)
	it.it = &NIterator{}
	return it
}

func (it *SampleIterator) Step() {
	it.it.Step()
}

func (it *SampleIterator) Iter(sample sampler, starting_power int) iter.Seq[*core.Results] {
	return func(yield func(*core.Results) bool) {
		var prev *core.Results
		for n := range it.it.Iter() {
			r := sample(n)
			if !yield(r) {
				return
			}
			m := core.MaxResults([]*core.Results{prev, r})
			if r == m {
				prev = m
			} else {
				it.Step()
			}
		}
	}
}

func FindMaximum2(sample sampler) *core.Results {
	it := NewSampleIterator()
	rs := slices.Collect(it.Iter(sample, 0))
	m := core.MaxResults(rs)
	return m
}

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
