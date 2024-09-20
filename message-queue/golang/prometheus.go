package golang

import "fmt"
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

func TestPusher(app *Instance) {
	fmt.Println("Testing prometheus push")
	TestMetric.Set(42)
	if err := app.Prometheus.Add(); err != nil {
		panic(fmt.Sprintf("Prometheus push failed: %v", err.Error()))
	}
}
