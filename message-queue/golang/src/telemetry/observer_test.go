package telemetry

import (
	"fmt"
	"testing"
)

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

type FakePusher struct {
	Result *core.Results
	SDesc  *core.SampleDesc
}

func (p *FakePusher) Push(sdesc core.SampleDesc, rs *core.Results) {
	p.Result = rs
	p.SDesc = &sdesc
}

func TestObserver(t *testing.T) {
	pub := core.NewPublisher()
	pusher := &FakePusher{nil, nil}
	o := newObserver(pusher)
	o.Observe(pub)

	sdesc := core.SampleDesc{4, "testing", "testing"}
	pub.Notify(core.SamplingWorkers, sdesc)

	rs := core.NewResults()
	rs.Add(core.NewWorkerResult(1, 10, 1_000_000_000))
	pub.Notify(core.SampleResults, rs)

	if pusher.Result != rs {
		t.Errorf("Result expected=%v got=%v", rs, pusher.Result)
	}

	if *pusher.SDesc != sdesc {
		t.Errorf("Result expected=%v got=%v", sdesc, *pusher.SDesc)
	}
}

func Test_newSDesc_panic(t *testing.T) {
	p := &FakePusher{nil, nil}
	o := newObserver(p)

	type TC struct {
		method func(core.Event, any)
		event  core.Event
		err    FailedToCastError
	}

	for _, tc := range []TC{
		TC{
			o.newSDesc,
			core.SamplingWorkers,
			FailedToCastError{4, "core.SampleDesc"},
		},
		TC{
			o.newResult,
			core.SampleResults,
			FailedToCastError{4, "*core.Results"},
		},
	} {
		test := func(t *testing.T) {
			defer func() {
				if r := recover(); r == nil {
					t.Errorf("Failed to panic")
				} else {
					if r != tc.err {
						t.Errorf("exp=%v got=%v", tc.err, r)
					}
				}
			}()

			tc.method(tc.event, 4)
		}
		t.Run(fmt.Sprintf("%v", tc), test)
	}
}
