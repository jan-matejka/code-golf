package main

import "fmt"
import "os"
import "context"
import "time"
import "sync"

import "github.com/jan-matejka/code-golf/message-queue/golang/src"
import "github.com/jan-matejka/code-golf/message-queue/golang/src/observer"

import "github.com/jackc/pgx/v4/pgxpool"

func die(message string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, message, args...)
	os.Exit(1)
}

func insert(pool *pgxpool.Pool, i int) {
	tx, err := pool.Begin(context.Background())
	if err != nil {
		die("Tx failed: %v\n", err)
	}

	_, err = tx.Exec(context.Background(), "insert into queue (data) values ($1)", fmt.Sprintf("%d", i))
	if err != nil {
		die("exec failed %v\n", err)
	}

	err = tx.Commit(context.Background())
	if err != nil {
		die("failed commit %v\n", err)
	}
}

func worker(wg *sync.WaitGroup, id int, quit <-chan bool, pool *pgxpool.Pool, end chan<- *jmcgmqp.WorkerResult) {
	wg.Done()
	wg.Wait()

	start := time.Now()

	var i int
	for {
		select {
		case <-quit:
			duration := time.Since(start)
			r := jmcgmqp.NewWorkerResult(id, i, duration)
			end <- r
			return
		default:
			insert(pool, i)
			i++
		}
	}
}

func sample_workers(app *jmcgmqp.Instance, workers int, pool *pgxpool.Pool) *jmcgmqp.Results {
	fmt.Printf("Spawning %d workers\n", workers)
	quit_channels := make([]chan bool, workers, workers)
	end_channels := make([]chan *jmcgmqp.WorkerResult, workers, workers)

	var wg sync.WaitGroup
	wg.Add(workers + 1)
	for i := range quit_channels {
		quit_channels[i] = make(chan bool)
		end_channels[i] = make(chan *jmcgmqp.WorkerResult)
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

	rs := jmcgmqp.NewResults()
	for i := range end_channels {
		r := <-end_channels[i]
		rs.Add(r)
	}

	rs.Print()
	return rs
}

func main() {
	app, err := jmcgmqp.NewInstance()
	if err != nil {
		die("Couldnt construct instance: %v\n", err)
	}
	if app.Config.Test_prometheus == 1 {
		jmcgmqp.TestPusher(app)
		os.Exit(0)
	}
	pool, err := pgxpool.Connect(context.Background(), "postgres://mq@localhost/mq?pool_max_conns=2048")
	if err != nil {
		die("Unable to connect to database: %v\n", err)
	}
	defer pool.Close()

	pgm, err := postgres.NewPgMetrics(app.Config)
	if err != nil {
		die("Unable to connect to postgres metrics: %v\n", err)
	}

	sample := func(workers int) *jmcgmqp.Results {
		r := sample_workers(app, workers, pool)
		sampleDesc := jmcgmqp.SampleDesc{workers, "goroutines", "postgres"}
		jmcgmqp.PushMetrics(app, sampleDesc, r)
		err := pgm.Push(context.Background(), app.Runtime, sampleDesc, r)
		if err != nil {
			die("%v", err)
		}
		return r
	}

	r := jmcgmqp.FindMaximum(sample)
	if r == nil {
		die("No successful run")
	}

	fmt.Printf("Found maximum:\n")
	r.Print()

	os.Exit(0)
}
