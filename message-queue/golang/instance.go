package golang

import "github.com/prometheus/client_golang/prometheus/push"

type Instance struct {
	Config     *Config
	Prometheus *push.Pusher
}

func NewInstance() (*Instance, error) {
	i := new(Instance)
	c, err := NewConfig()
	if err != nil {
		return nil, err
	}
	i.Config = c

	i.Prometheus = NewPusher()
	return i, nil
}
