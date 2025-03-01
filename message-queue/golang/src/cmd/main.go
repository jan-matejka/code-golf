package main

import "fmt"
import "os"
import "context"

import "github.com/jan-matejka/code-golf/message-queue/golang/src"
import "github.com/jan-matejka/code-golf/message-queue/golang/src/telemetry"
import "github.com/jan-matejka/code-golf/message-queue/golang/src/sampler"

import "github.com/jackc/pgx/v4/pgxpool"

func main() {
	app, err := jmcgmqp.NewInstance()
	if err != nil {
		jmcgmqp.Die("Couldnt construct instance: %v\n", err)
	}
	if app.Config.Test_prometheus == 1 {
		app.Prometheus.TestPush()
		os.Exit(0)
	}
	pool, err := pgxpool.Connect(context.Background(), "postgres://mq@localhost/mq?pool_max_conns=2048")
	if err != nil {
		jmcgmqp.Die("Unable to connect to database: %v\n", err)
	}
	defer pool.Close()

	pgm, err := telemetry.NewPgMetrics(app.Config)
	if err != nil {
		jmcgmqp.Die("Unable to connect to postgres metrics: %v\n", err)
	}

	sampler := sampler.NewSampler(pgm, app, pool)
	app.Prometheus.Observe(sampler.Observable)

	r := jmcgmqp.FindMaximum(sampler.Run)
	if r == nil {
		jmcgmqp.Die("No successful run")
	}

	fmt.Printf("Found maximum:\n")
	r.Print()

	os.Exit(0)
}
