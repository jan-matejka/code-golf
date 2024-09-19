package golang

import "github.com/prometheus/client_golang/prometheus"
import "github.com/prometheus/client_golang/prometheus/push"

var (
	TestMetric = prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "test",
		Help: "Test metric",
	})
)

func NewPusher() *push.Pusher {
	registry := prometheus.NewRegistry()
	registry.MustRegister(TestMetric)

	pusher := push.New("http://localhost:9091", "mq-producer").Gatherer(registry)
	return pusher
}
