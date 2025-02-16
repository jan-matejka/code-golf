package telemetry

import (
	"context"
	"reflect"
	"testing"
)

import "github.com/jackc/pgx/v4/pgxpool"
import "github.com/jan-matejka/code-golf/message-queue/golang/src"
import "github.com/jan-matejka/code-golf/message-queue/golang/src/test"

func mkTestDb() (*pgxpool.Pool, error) {
	ctx := context.Background()
	pool, err := pgxpool.Connect(ctx, test.TestConfig.TelemetryPostgresRoot)
	if err != nil {
		return nil, err
	}
	defer pool.Close()

	_, err = pool.Exec(ctx, "drop database if exists test")
	if err != nil {
		return nil, err
	}
	_, err = pool.Exec(ctx, "create database test template mq")
	if err != nil {
		return nil, err
	}

	pool, err = pgxpool.Connect(ctx, test.TestConfig.TelemetryPostgresMq)
	if err != nil {
		return nil, err
	}
	return pool, nil
}

func TestPush(t *testing.T) {
	pool, err := mkTestDb()
	if err != nil {
		t.Fatalf("%v", err)
	}
	defer pool.Close()

	cg := jmcgmqp.DefaultConfig()
	cg.TelemetryPostgres = test.TestConfig.TelemetryPostgresMq

	pgm, err := NewPgMetrics(cg)
	if err != nil {
		t.Fatalf("%v", err)
	}

	r, err := jmcgmqp.NewRuntime()
	if err != nil {
		t.Fatalf("%v", err)
	}

	sdesc := jmcgmqp.SampleDesc{2, "goroutines", "postgres"}
	results := jmcgmqp.NewResults()
	results.Add(jmcgmqp.NewWorkerResult(1, 10, 20))
	results.Add(jmcgmqp.NewWorkerResult(2, 30, 40))

	ctx := context.Background()
	err = pgm.Push(ctx, r, sdesc, results)
	if err != nil {
		t.Fatalf("%v", err)
	}

	q := `
  select
    id,ctime,uuid,lang,lang_version,runtime,os,kernel,arch
  from results.runtime
  `
	rows, err := pool.Query(ctx, q)
	defer rows.Close()
	i := 0
	var runtime_id int
	var r2 jmcgmqp.Runtime
	for rows.Next() {
		err = rows.Scan(
			&runtime_id,
			&r2.Ctime,
			&r2.Uuid,
			&r2.Lang,
			&r2.Lang_version,
			&r2.Runtime,
			&r2.Os,
			&r2.Kernel,
			&r2.Arch,
		)
		if err != nil {
			t.Fatalf("%v", err)
		}
		i += 1
	}
	if i != 1 {
		t.Fatalf("unexpcted rows, expected: 1, got: %v", i)
	}
	expected := r.Map()
	got := r2.Map()
	delete(expected, "ctime") // expected has tz, while got is tzless
	delete(got, "ctime")
	if !reflect.DeepEqual(expected, got) {
		t.Fatalf("expected %v, got %v", expected, got)
	}

	q = `
  select
    id,runtime_id,n_workers,algorithm,mq_system
  from results.sample
  `
	rows, err = pool.Query(ctx, q)
	if err != nil {
		t.Fatalf("%v", err)
	}
	defer rows.Close()
	i = 0
	var sample_id int
	var r_id int
	var s2 jmcgmqp.SampleDesc
	for rows.Next() {
		i += 1
		err = rows.Scan(&sample_id, &r_id, &s2.N_workers, &s2.Algorithm, &s2.Mq_system)
		if err != nil {
			t.Fatalf("%v", err)
		}
	}
	if i != 1 {
		t.Fatalf("unexpected rows, expected: 1, got: %v", i)
	}
	if r_id != runtime_id {
		t.Fatalf("Expected %v, got %v", runtime_id, r_id)
	}
	if !reflect.DeepEqual(sdesc, s2) {
		t.Fatalf("expected %v, got %v", sdesc, s2)
	}

	q = `
  select
    id,sample_id,worker_id,messages_total,duration_ns
  from results.worker
  `
	rows, err = pool.Query(ctx, q)
	if err != nil {
		t.Fatalf("%v", err)
	}
	defer rows.Close()
	var workers [][]interface{}
	for rows.Next() {
		vals, err := rows.Values()
		if err != nil {
			t.Fatalf("%v", err)
		}
		workers = append(workers, vals)
	}

	var expected2 [][]interface{}
	for i, wr := range results.Workers {
		expected2 = append(
			expected2,
			[]interface{}{
				int32(i + 1),
				int32(sample_id),
				int32(wr.WorkerId),
				int32(wr.MessagesTotal),
				int64(wr.DurationNs),
			},
		)
	}

	if !reflect.DeepEqual(expected2, workers) {
		// The DeepEqual might fail because of different integer types but it shows up the same under
		// %v. If that happens, the values need to be examined individually under %v and %T.
		// And this is why we cast the expected2 values above.
		t.Fatalf("expected %v, got %v", expected2, workers)
	}
}
