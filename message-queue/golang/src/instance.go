package jmcgmqp

import "github.com/prometheus/client_golang/prometheus/push"
import "github.com/jan-matejka/code-golf/message-queue/golang/src/core"

type Instance struct {
	Runtime    *core.Runtime
	Config     *Config
	Prometheus *push.Pusher
}

func NewInstance() (*Instance, error) {
	runtime, err := core.NewRuntime()
	if err != nil {
		return nil, err
	}
	i := new(Instance)
	i.Runtime = runtime
	c, err := NewConfig()
	if err != nil {
		return nil, err
	}
	i.Config = c

	i.Prometheus = NewPusher()
	return i, nil
}
