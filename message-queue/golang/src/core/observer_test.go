package core

import (
	"fmt"
	"reflect"
	"testing"
)

func Test(t *testing.T) {
	p := NewPublisher()

	xs := []*Results{}

	observer := func(e Event, data any) {
		r, ok := data.(*Results)
		if !ok {
			fmt.Printf("Failed to cast data=%v to *Results\n", data)
			return
		}

		xs = append(xs, r)
	}

	p.Register(SampleResults, observer)

	rs := NewResults()
	rs.Add(NewWorkerResult(1, 10, 1_000_000_000))

	p.Notify(SampleResults, rs)

	expected := []*Results{rs}
	if !reflect.DeepEqual(xs, expected) {
		t.Errorf("got xs=%v != expected=%v", xs, expected)
	}
}
