package golang

import "fmt"
import "github.com/prometheus/client_golang/prometheus"
import "github.com/prometheus/client_golang/prometheus/push"
import "golang.org/x/exp/maps"

var (
	TestMetric = prometheus.NewGaugeVec(prometheus.GaugeOpts{
		Name: "test",
		Help: "Test metric",
	},
		append(append([]string{"worker_id"}, RuntimeFieldNames()...), SampleDescFieldNames()...),
	)
)

func NewPusher() *push.Pusher {
	registry := prometheus.NewRegistry()
	registry.MustRegister(TestMetric)

	pusher := push.New("http://localhost:9091", "mq-producer").Gatherer(registry)
	return pusher
}

func TestPusher(app *Instance) {
	fmt.Println("Testing prometheus push")
	str_labels := app.Runtime.Map()
	maps.Copy(str_labels, map[string]string{"worker_id": "0"})
	maps.Copy(str_labels, SampleDesc{n_workers: 8, algorithm: "channels", mq_system: "postgres"}.Map())
	var labels prometheus.Labels = str_labels
	TestMetric.With(labels).Set(42)
	if err := app.Prometheus.Add(); err != nil {
		panic(fmt.Sprintf("Prometheus push failed: %v", err.Error()))
	}
}
