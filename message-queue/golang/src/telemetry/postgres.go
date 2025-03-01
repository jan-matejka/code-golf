package telemetry

import (
	"context"
)

import (
	"github.com/jackc/pgx/v4"
	"github.com/jackc/pgx/v4/pgxpool"
)

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

type PgMetrics struct {
	pool       *pgxpool.Pool
	runtime_id int
}

func NewPgMetrics(cg *core.Config) (*PgMetrics, error) {
	pool, err := pgxpool.Connect(context.Background(), cg.TelemetryPostgres)
	if err != nil {
		return nil, err
	}

	pgm := &PgMetrics{pool, 0}
	return pgm, nil
}

func (p *PgMetrics) Push(
	ctx context.Context,
	runtime *core.Runtime,
	sample core.SampleDesc,
	rs *core.Results,
) error {
	tx, err := p.pool.Begin(ctx)
	if err != nil {
		return err
	}

	if p.runtime_id == 0 {
		runtime_id, err := runtimeId(ctx, tx, runtime)
		if err != nil {
			return err
		}
		p.runtime_id = runtime_id
	}

	sample_id, err := sampleId(ctx, tx, p.runtime_id, sample)
	if err != nil {
		return err
	}

	for _, r := range rs.Workers {
		err = workerResult(ctx, tx, sample_id, r)
		if err != nil {
			return err
		}
	}

	err = tx.Commit(ctx)
	if err != nil {
		return err
	}
	return nil
}

func runtimeId(ctx context.Context, tx pgx.Tx, r *core.Runtime) (int, error) {
	q := `
  insert into results.runtime (
      ctime, uuid, lang, lang_version, runtime, os, kernel, arch
  ) values (
      $1, $2, $3, $4, $5, $6, $7, $8
  )
  returning id;
  `
	rows, err := tx.Query(
		ctx,
		q,
		r.Ctime,
		r.Uuid,
		r.Lang,
		r.Lang_version,
		r.Runtime,
		r.Os,
		r.Kernel,
		r.Arch,
	)
	if err != nil {
		return 0, err
	}
	defer rows.Close()

	var id int
	rows.Next()
	err = rows.Scan(&id)
	if err != nil {
		return 0, err
	}
	return id, nil
}

func sampleId(ctx context.Context, tx pgx.Tx, runtime_id int, sample core.SampleDesc) (int, error) {
	// TBD: pgxv5 supports named arguments
	q := `
  with
  sel as (
      select id from results.sample where
          runtime_id = $1
          and n_workers = $2
          and algorithm = $3
          and mq_system = $4
  ),
  ins as (
      insert into results.sample
      (runtime_id, n_workers, algorithm, mq_system)
      values
      ($1, $2, $3, $4)
      on conflict do nothing
      returning id
  )
  select * from ins
  union
  select * from sel
  where id is not null;
  `
	var id int
	err := tx.QueryRow(
		ctx,
		q,
		runtime_id,
		sample.N_workers,
		sample.Algorithm,
		sample.Mq_system,
	).Scan(&id)
	if err != nil {
		return 0, err
	}
	return id, nil
}

func workerResult(ctx context.Context, tx pgx.Tx, sample_id int, wr *core.WorkerResult) error {
	q := `
  insert into results.worker
  (sample_id, worker_id, messages_total, duration_ns)
  values
  ($1, $2, $3, $4)
  `
	_, err := tx.Exec(
		ctx,
		q,
		sample_id,
		wr.WorkerId,
		wr.MessagesTotal,
		wr.DurationNs,
	)
	return err
}

func (p *PgMetrics) Close() {
	p.pool.Close()
}
