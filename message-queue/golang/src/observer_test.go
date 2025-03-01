package jmcgmqp

import (
	"fmt"
	"reflect"
	"testing"
)

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

func Test(t *testing.T) {
	p := NewPublisher()

	xs := []*core.Results{}

	observer := func(e Event, data any) {
		r, ok := data.(*core.Results)
		if !ok {
			fmt.Printf("Failed to cast data=%v to *Results\n", data)
			return
		}

		xs = append(xs, r)
	}

	p.Register(SampleResults, observer)

	rs := core.NewResults()
	rs.Add(core.NewWorkerResult(1, 10, 1_000_000_000))

	p.Notify(SampleResults, rs)

	expected := []*core.Results{rs}
	if !reflect.DeepEqual(xs, expected) {
		t.Errorf("got xs=%v != expected=%v", xs, expected)
	}
}
