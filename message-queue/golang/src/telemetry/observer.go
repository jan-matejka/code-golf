package telemetry

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

type observer struct {
	p     Pusher
	sdesc core.SampleDesc
}

func newObserver(p Pusher) *observer {
	return &observer{p, core.NullSampleDesc()}
}

func (o *observer) Observe(p *core.Publisher) {
	p.Register(core.SamplingWorkers, o.newSDesc)
	p.Register(core.SampleResults, o.newResult)
}

func (o *observer) newSDesc(e core.Event, data any) {
	sdesc, ok := data.(core.SampleDesc)
	if !ok {
		panic(core.FailedToCastError{data, "core.SampleDesc"})
	}
	o.sdesc = sdesc
}

func (o *observer) newResult(e core.Event, data any) {
	r, ok := data.(*core.Results)
	if !ok {
		panic(core.FailedToCastError{data, "*core.Results"})
	}
	o.p.Push(o.sdesc, r)
}
