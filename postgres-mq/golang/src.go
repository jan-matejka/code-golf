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

func worker(id int, quit chan bool, wg *sync.WaitGroup, pool *pgxpool.Pool, end chan int) {
	defer wg.Done()
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

func main() {
	pool, err := pgxpool.Connect(context.Background(), "postgres://mq@localhost/mq")
	if err != nil {
		die("Unable to connect to database: %v\n", err)
	}
	defer pool.Close()
	var wg sync.WaitGroup
	var quit_channels [4]chan bool
	var end_channels [4]chan int

	for i := range quit_channels {
		wg.Add(1)
		quit_channels[i] = make(chan bool)
		end_channels[i] = make(chan int)
		go worker(i, quit_channels[i], &wg, pool, end_channels[i])
	}

	time.Sleep(time.Second)

	for i := range quit_channels {
		quit_channels[i] <- true
	}

	var n, total int
	for i := range end_channels {
		n = <-end_channels[i]
		fmt.Println(n)
		total += n
	}

	// do I need the wait group since I am already waiting for the end results?
	wg.Wait()

	fmt.Printf("total: %d\n", total)
	fmt.Printf("total ips: %f\n", float32(total))
	os.Exit(0)
}
