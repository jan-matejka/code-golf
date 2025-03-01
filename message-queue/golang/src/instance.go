package jmcgmqp

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
	"github.com/jan-matejka/code-golf/message-queue/golang/src/telemetry"
)

type Instance struct {
	Runtime    *core.Runtime
	Config     *core.Config
	Prometheus telemetry.PrometheusMetrics
}

func NewInstance() (*Instance, error) {
	runtime, err := core.NewRuntime()
	if err != nil {
		return nil, err
	}
	i := new(Instance)
	i.Runtime = runtime
	c, err := core.NewConfig()
	if err != nil {
		return nil, err
	}
	i.Config = c

	i.Prometheus = telemetry.NewPrometheusMetrics(runtime)
	return i, nil
}
