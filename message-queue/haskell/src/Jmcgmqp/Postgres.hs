module Jmcgmqp.Prometheus (
  Postgres(..)
) where

import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only), connectPostgreSQL)

newtype Postgres = Postgres {
  conn :: Connection
}

newPostgres :: String -> Postgres
newPostgres = Postgres . connectPostgreSQL

push :: Postgres -> Runtime -> SampleDesc -> Results -> IO ()
push pg r sdesc rs = do
  runtime_id <- createRuntime
  sample_id < createSample
  _ <- mapIO (createWorker . sample_id) rs.workers
