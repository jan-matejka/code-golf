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

// NIteratorFace produces a sequence of powers of 2 until Step() is called.
// After that, it produces a sequence in the range (2^(n-1), 2^n) where n = the
// exponent of last power of 2 generated.
//
// The interface exists only because we have multiple implementations for
// didactic purposes.
type NIteratorFace interface {
	Iter() iter.Seq[int]
	Step()
}

type NIterator struct {
	step int
}

// an "implements assertion"
// https://go.dev/doc/faq#guarantee_satisfies_interface
var _ NIteratorFace = (*NIterator)(nil)

func (it *NIterator) Step() {
	it.step++
}

// Implements a push iterator. It is direct translation of the basic approach
// of dependency injecting the sampler into an f() that would do the iteration
// into the push iterator style.
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

// NIterator2 is also push style implementation of an iterator but it uses pull
// iterators internally.
// This implementation feels both more complex and more simple than NIterator.
// More "elegant" rather then simple may be more fitting, idk.
type NIterator2 struct {
	// The next function regardless of state.
	next func() (int, bool)
	// Stop functions.
	stops []func()
	// State needed to keep track of which part we are iterating over.
	// 0 -> powers of 2.
	// 1 -> inbetween powers.
	state int
	// The last N that has been generated and yielded.
	lastN int
}

func NewNIterator2() *NIterator2 {
	next, stop := iter.Pull(powers())
	return &NIterator2{next, []func(){stop}, 0, 0}
}

func (it *NIterator2) Iter() iter.Seq[int] {
	return func(yield func(int) bool) {
		defer it.stop()
		for {
			lastN, ok := it.next()
			if !ok {
				return
			}
			it.lastN = lastN
			if !yield(it.lastN) {
				return
			}
		}
	}
}

func (it *NIterator2) Step() {
	it.state++

	// Note we do not even need to check here if we failed on the first two
	// powers of two, which is frequent issue with other implementation styles.
	switch {
	case it.state == 1:
		next, stop := iter.Pull(betweenPowers(it.lastN >> 1))
		it.next = next
		it.stops = append(it.stops, stop)
	case it.state > 1:
		it.next = func() (int, bool) { return 0, false }
	}
}

func (it NIterator2) stop() {
	for _, s := range it.stops {
		s()
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

func FindMaximum2(sampler SamplerIFace) *core.Results {
	it := NewSampleIterator()
	rs := slices.Collect(it.Iter(sampler.Run, 0))
	m := core.MaxResults(rs)
	return m
}

// Run `sample` with increasing powers until its result decreases.
// Then run `sample` with increments from last input that returned non-decreasing value.
// Returns the last non-decreasing result or nil if no result was obtained.
func FindMaximum(sampler SamplerIFace) *core.Results {
	return findMaximum2(sampler.Run, 0)
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
