package telemetry

import (
	"fmt"
	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/push"
	"golang.org/x/exp/maps"
	"strconv"
	"time"
)

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

var (
	TestMetric = prometheus.NewGaugeVec(prometheus.GaugeOpts{
		Name: "test",
		Help: "Test metric",
	},
		append(append([]string{"worker_id"}, core.RuntimeFieldNames()...), core.SampleDescFieldNames()...),
	)

	MessagesTotal = prometheus.NewGaugeVec(prometheus.GaugeOpts{
		Name: "messages_total",
		Help: "Messages sent",
	},
		append(append([]string{"worker_id"}, core.RuntimeFieldNames()...), core.SampleDescFieldNames()...),
	)

	MessagesPerSecond = prometheus.NewGaugeVec(prometheus.GaugeOpts{
		Name: "messages_per_second",
		Help: "Messages per second sent",
	},
		append(append([]string{"worker_id"}, core.RuntimeFieldNames()...), core.SampleDescFieldNames()...),
	)

	DurationSeconds = prometheus.NewGaugeVec(prometheus.GaugeOpts{
		Name: "duration_seconds",
		Help: "Work duration in seconds",
	},
		append(append([]string{"worker_id"}, core.RuntimeFieldNames()...), core.SampleDescFieldNames()...),
	)
)

func newPusher() *push.Pusher {
	registry := prometheus.NewRegistry()
	registry.MustRegister(TestMetric)
	registry.MustRegister(MessagesTotal)
	registry.MustRegister(MessagesPerSecond)
	registry.MustRegister(DurationSeconds)

	pusher := push.New("http://localhost:9091", "mq-producer").Gatherer(registry)
	return pusher
}

func mkLabels(r *core.Runtime, sample core.SampleDesc, worker *core.WorkerResult) prometheus.Labels {
	str_labels := r.Map()
	maps.Copy(str_labels, map[string]string{"worker_id": strconv.Itoa(worker.WorkerId)})
	maps.Copy(str_labels, sample.Map())
	var labels prometheus.Labels = str_labels
	return labels
}

type PrometheusMetrics struct {
	pusher  *push.Pusher
	runtime *core.Runtime
}

func NewPrometheusMetrics(r *core.Runtime) PrometheusMetrics {
	pm := PrometheusMetrics{newPusher(), r}
	return pm
}

func (pm PrometheusMetrics) Push(sample core.SampleDesc, rs *core.Results) {
	for _, r := range rs.Workers {
		MessagesTotal.With(mkLabels(pm.runtime, sample, r)).Set(float64(r.MessagesTotal))
		MessagesPerSecond.With(mkLabels(pm.runtime, sample, r)).Set(r.MessagesPerSecond)
		DurationSeconds.With(mkLabels(pm.runtime, sample, r)).Set(r.DurationSeconds)

		if err := pm.pusher.Add(); err != nil {
			panic(fmt.Sprintf("Prometheus push failed: %v", err.Error()))
		}
	}
}

func (pm PrometheusMetrics) TestPush() {
	fmt.Println("Testing prometheus push")
	result := core.NewWorkerResult(0, 1, time.Second)
	labels := mkLabels(pm.runtime, core.SampleDesc{8, "goroutines", "postgres"}, result)
	TestMetric.With(labels).Set(42)
	if err := pm.pusher.Add(); err != nil {
		panic(fmt.Sprintf("Prometheus push failed: %v", err.Error()))
	}
}
