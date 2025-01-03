package main

import "fmt"
import "os"
import "context"
import "time"
import "sync"

import "github.com/jan-matejka/code-golf/message-queue/golang"
import "github.com/jan-matejka/code-golf/message-queue/golang/observer"

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

func worker(wg *sync.WaitGroup, id int, quit <-chan bool, pool *pgxpool.Pool, end chan<- *golang.WorkerResult) {
	wg.Done()
	wg.Wait()

	start := time.Now()

	var i int
	for {
		select {
		case <-quit:
			duration := time.Since(start)
			r := golang.NewWorkerResult(id, i, duration)
			end <- r
			return
		default:
			insert(pool, i)
			i++
		}
	}
}

func sample_workers(app *golang.Instance, workers int, pool *pgxpool.Pool) *golang.Results {
	fmt.Printf("Spawning %d workers\n", workers)
	quit_channels := make([]chan bool, workers, workers)
	end_channels := make([]chan *golang.WorkerResult, workers, workers)

	var wg sync.WaitGroup
	wg.Add(workers + 1)
	for i := range quit_channels {
		quit_channels[i] = make(chan bool)
		end_channels[i] = make(chan *golang.WorkerResult)
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

	rs := golang.NewResults()
	for i := range end_channels {
		r := <-end_channels[i]
		rs.Add(r)
	}

	rs.Print()
	return rs
}

func main() {
	app, err := golang.NewInstance()
	if err != nil {
		die("Couldnt construct instance: %v\n", err)
	}
	if app.Config.Test_prometheus == 1 {
		golang.TestPusher(app)
		os.Exit(0)
	}
	pool, err := pgxpool.Connect(context.Background(), "postgres://mq@localhost/mq?pool_max_conns=2048")
	if err != nil {
		die("Unable to connect to database: %v\n", err)
	}
	defer pool.Close()

	pgm, err := postgres.NewPgMetrics("postgres://mq@localhost/mq?pool_max_conns=2048")
	if err != nil {
		die("Unable to connect to postgres metrics: %v\n", err)
	}

	sample := func(workers int) *golang.Results {
		r := sample_workers(app, workers, pool)
		sampleDesc := golang.SampleDesc{workers, "goroutines", "postgres"}
		golang.PushMetrics(app, sampleDesc, r)
		err := pgm.Push(context.Background(), app.Runtime, sampleDesc, r)
		if err != nil {
			die("%v", err)
		}
		return r
	}

	r := golang.FindMaximum(sample)
	if r == nil {
		die("No successful run")
	}

	fmt.Printf("Found maximum:\n")
	r.Print()

	os.Exit(0)
}
