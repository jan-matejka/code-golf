package main

import "fmt"
import "os"
import "sync"
import "context"
import "time"
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

func worker(id int, quit chan bool, wg *sync.WaitGroup, pool *pgxpool.Pool) {
	defer wg.Done()
	var i int
	for {
		select {
		case <-quit:
			return
		default:
			insert(pool, i)
			i++
		}
	}
}

func main() {
	pool, err := pgxpool.Connect(context.Background(), "postgres://mq@localhost/mq")
	if err != nil {
		die("Unable to connect to database: %v\n", err)
	}
	defer pool.Close()
	var wg sync.WaitGroup
	var quit_channels [4]chan bool

	for i := range quit_channels {
		wg.Add(1)
		quit_channels[i] = make(chan bool)
		go worker(i, quit_channels[i], &wg, pool)
	}

	time.Sleep(time.Second)

	for i := range quit_channels {
		quit_channels[i] <- true
	}

	print("waiting")
	wg.Wait()
	os.Exit(0)
}
