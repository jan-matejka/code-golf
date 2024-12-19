module Jmcgmqp.Postgres (
  Postgres(..)
) where

import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only), connectPostgreSQL)

import Jmcgmqp.Runtime (Runtime)
import Jmcgmqp.Prometheus.Metrics (SampleDesc)
import Jmcgmqp.Worker (Results)

newtype Postgres = Postgres {
  conn :: Connection
}

newPostgres :: String -> IO Postgres
newPostgres = Postgres . connectPostgreSQL

push :: Postgres -> Runtime -> SampleDesc -> Results -> IO ()
push pg r sdesc rs = do
  runtime_id <- createRuntime
  sample_id < createSample
  mapM_ (createWorker . sample_id) rs.workers
