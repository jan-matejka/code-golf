-- show mps of each sample
select
  r.id as runtime_id,
  s.id as sample_id,
  r.lang,
  s.n_workers,
  s.n_workers * 1/power(10,-9)*sum(w.messages_total)/sum(w.duration_ns) as mps,
  s.algorithm,
  s.mq_system,
  r.runtime,
  r.lang_version
from results.runtime r
join results.sample s on (s.runtime_id = r.id)
join results.worker w on (w.sample_id = s.id)
group by (r.id, s.id)
order by mps desc
