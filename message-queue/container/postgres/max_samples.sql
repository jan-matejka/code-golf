-- show maximum sample per runtime.
-- this will need adjustment if we are gonna record multiple algorithms or mq_systems per runtime.
-- Perhaps this suggests the algorithm and mq_system attributes should go to runtime instead of sample.
with samples as (
  select
    r.id as runtime_id,
    s.id as sample_id,
    1/power(10,-9)*sum(w.messages_total)/sum(w.duration_ns) as mps
  from results.runtime r
  join results.sample s on (s.runtime_id = r.id)
  join results.worker w on (w.sample_id = s.id)
  group by (r.id, s.id)
),
runtimes as (
  select runtime_id, max(mps) as mps from samples group by runtime_id
)
select
  r.mps,
  r.runtime_id,
  s.sample_id,
  s2.n_workers as time,
  r2.lang,
  s2.algorithm,
  s2.mq_system,
  r2.runtime,
  r2.lang_version
from runtimes r
join samples s on (
  s.runtime_id = r.runtime_id and r.mps = s.mps
)
join results.runtime r2 on (r2.id = r.runtime_id)
join results.sample s2 on (s2.id = s.sample_id)
order by mps desc
;
