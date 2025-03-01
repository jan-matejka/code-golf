package jmcgmqp

import (
	"context"
	"fmt"
	"github.com/jackc/pgx/v4/pgxpool"
	"sync"
	"time"
)

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

func insert(pool *pgxpool.Pool, i int) {
	tx, err := pool.Begin(context.Background())
	if err != nil {
		Die("Tx failed: %v\n", err)
	}

	_, err = tx.Exec(context.Background(), "insert into queue (data) values ($1)", fmt.Sprintf("%d", i))
	if err != nil {
		Die("exec failed %v\n", err)
	}

	err = tx.Commit(context.Background())
	if err != nil {
		Die("failed commit %v\n", err)
	}
}

func worker(wg *sync.WaitGroup, id int, quit <-chan bool, pool *pgxpool.Pool, end chan<- *core.WorkerResult) {
	wg.Done()
	wg.Wait()

	start := time.Now()

	var i int
	for {
		select {
		case <-quit:
			duration := time.Since(start)
			r := core.NewWorkerResult(id, i, duration)
			end <- r
			return
		default:
			insert(pool, i)
			i++
		}
	}
}

func sample_workers(app *Instance, workers int, pool *pgxpool.Pool) *core.Results {
	fmt.Printf("Spawning %d workers\n", workers)
	quit_channels := make([]chan bool, workers, workers)
	end_channels := make([]chan *core.WorkerResult, workers, workers)

	var wg sync.WaitGroup
	wg.Add(workers + 1)
	for i := range quit_channels {
		quit_channels[i] = make(chan bool)
		end_channels[i] = make(chan *core.WorkerResult)
		go worker(&wg, i, quit_channels[i], pool, end_channels[i])
	}

	wg.Done()
	wg.Wait()

	fmt.Printf("Waiting\n")
	for i := app.Config.Duration; i > 0; i-- {
		fmt.Printf("%d\n", i)
		time.Sleep(time.Second)
	}

	for i := range quit_channels {
		quit_channels[i] <- true
	}

	rs := core.NewResults()
	for i := range end_channels {
		r := <-end_channels[i]
		rs.Add(r)
	}

	rs.Print()
	return rs
}

type SamplerIFace interface {
	Run(int) *core.Results
	Observable() *core.Publisher
}

type Sampler struct {
	app        *Instance
	pool       *pgxpool.Pool
	observable *core.Publisher
}

func NewSampler(app *Instance, pool *pgxpool.Pool) *Sampler {
	return &Sampler{app, pool, core.NewPublisher()}
}

func (s Sampler) Observable() *core.Publisher {
	return s.observable
}

func (s Sampler) Run(n int) *core.Results {
	sampleDesc := core.SampleDesc{n, "goroutines", "postgres"}
	s.observable.Notify(core.SamplingWorkers, sampleDesc)
	r := sample_workers(s.app, n, s.pool)
	s.observable.Notify(core.SampleResults, r)
	return r
}
