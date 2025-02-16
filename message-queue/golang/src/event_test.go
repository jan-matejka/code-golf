package jmcgmqp

import (
	"fmt"
	"testing"
)

func TestEvents(t *testing.T) {
	type TC struct {
		e     Event
		index int
		name  string
	}

	test := func(tc TC) func(t *testing.T) {
		return func(t *testing.T) {
			s := tc.e.String()
			if s != tc.name {
				t.Errorf("name got %v expected %v", s, tc.name)
			}

			i := tc.e.EnumIndex()
			if i != tc.index {
				t.Errorf("EnumIndex got %v expected %v", i, tc.index)
			}
		}
	}

	for _, tc := range []TC{
		TC{SampleResults, 1, "SampleResults"},
		TC{SamplingWorkers, 2, "SamplingWorkers"},
	} {
		t.Run(fmt.Sprintf("%v", tc), test(tc))
	}
}
