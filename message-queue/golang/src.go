package main

import "fmt"
import "math"
import "os"
import "context"
import "time"
import "github.com/jackc/pgx/v4/pgxpool"
import "sync"

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

func worker(wg *sync.WaitGroup, id int, quit chan bool, pool *pgxpool.Pool, end chan int) {
	wg.Done()
	wg.Wait()
	var i int
	for {
		select {
		case <-quit:
			end <- i
			return
		default:
			insert(pool, i)
			i++
		}
	}
}

func sample_workers(workers int, pool *pgxpool.Pool) float64 {
	fmt.Printf("Spawning %d workers\n", workers)
	quit_channels := make([]chan bool, workers, workers)
	end_channels := make([]chan int, workers, workers)

	var wg sync.WaitGroup
	wg.Add(workers + 1)
	for i := range quit_channels {
		quit_channels[i] = make(chan bool)
		end_channels[i] = make(chan int)
		go worker(&wg, i, quit_channels[i], pool, end_channels[i])
	}

	wg.Done()
	wg.Wait()
	var start = time.Now()
	time.Sleep(time.Second * 3)

	for i := range quit_channels {
		quit_channels[i] <- true
	}

	var n, total int
	for i := range end_channels {
		n = <-end_channels[i]
		fmt.Printf("%d: %d\n", i, n)
		total += n
	}

	elapsed := time.Since(start)

	fmt.Printf("total: %d\n", total)
	ips := float64(float64(total) / (float64(elapsed) * math.Pow10(-9)))
	fmt.Printf("total ips: %f\n", ips)
	return ips
}

func main() {
	pool, err := pgxpool.Connect(context.Background(), "postgres://mq@localhost/mq?pool_max_conns=2048")
	if err != nil {
		die("Unable to connect to database: %v\n", err)
	}
	defer pool.Close()

	var workers int
	var last_ips float64
	var i int
	for ; ; i++ {
		workers = 1 << i
		ips := sample_workers(workers, pool)
		if last_ips != 0 && last_ips >= ips {
			i--
			break
		} else {
			last_ips = ips
		}
	}

	for workers = 1<<i + 1; workers < 1<<(i+1); workers++ {
		ips := sample_workers(workers, pool)
		if last_ips != 0 && last_ips >= ips {
			fmt.Println("Done")
			break
		}
	}

	os.Exit(0)
}
