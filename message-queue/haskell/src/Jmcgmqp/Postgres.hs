module Jmcgmqp.Postgres (
  Postgres(..)
, newPostgres
, push
) where

import Control.Monad (void)
import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only), connectPostgreSQL, query, Query)
import Data.ByteString (ByteString)
import System.Clock (toNanoSecs, TimeSpec)

import Jmcgmqp.Runtime (Runtime(..))
import Jmcgmqp.Prometheus.Metrics (SampleDesc(..))
import Jmcgmqp.Worker (Results(..), WorkerResult(..))

newtype Postgres = Postgres {
  conn :: Connection
}

newPostgres :: ByteString -> IO Postgres
newPostgres x = do
  c <- connectPostgreSQL x
  return $ Postgres c

push :: Postgres -> Runtime -> SampleDesc -> Results -> IO ()
push pg r sdesc rs = do
  runtime_id <- createRuntime pg r
  sample_id <- createSample pg runtime_id sdesc
  mapM_ (createWorker pg sample_id) (rs.workers::[WorkerResult])

createRuntime :: Postgres -> Runtime -> IO Int
createRuntime pg r = do
  let q = read $ unlines
        [ "insert into results.runtime ("
        , "  ctime, uuid, lang, lang_version, runtime, os, kernel, arch"
        , ") values ("
        , " ?, ?, ?, ?, ?, ?, ?, ?"
        , ")"
        , "returning id"
        ] :: Query

  [Only id]
    <- query pg.conn q
    ( r.ctime
    , r.uuid
    , r.lang
    , r.lang_version
    , r.runtime
    , r.os
    , r.kernel
    , r.arch
    )
  return id

createSample :: Postgres -> Int -> SampleDesc -> IO Int
createSample pg runtime_id sdesc = do
  let q = read $ unlines
        [ "with"
        , "sel as ("
        , "  select id from results.sample where"
        , "      runtime_id = ?"
        , "      and n_workers = ?"
        , "      and algorithm = ?"
        , "      and mq_system = ?"
        , "),"
        , "ins as ("
        , "  insert into results.sample"
        , "  (runtime_id, n_workers, algorithm, mq_system)"
        , "  values"
        , "  (?, ?, ?, ?)"
        , "  on conflict do nothing"
        , "  returning id"
        , ")"
        , "select * from ins"
        , "union"
        , "select * from sel"
        , "where id is not null;"
        ] :: Query
  [Only id]
    <- query pg.conn q
    ( runtime_id
    , sdesc.nWorkers
    , sdesc.algorithm
    , sdesc.mqSystem
    , runtime_id
    , sdesc.nWorkers
    , sdesc.algorithm
    , sdesc.mqSystem
    )
  return id

createWorker :: Postgres -> Int -> WorkerResult -> IO ()
createWorker pg sample_id w =
  void $ execute pg.conn q
    (sample_id, w.workerId, w.messagesTotal, (toNanoSecs w.duration))
  where
    q = read $ unlines 
      [ "insert into results.worker"
      , "(sample_id, worker_id, messages_total, duration_ns)"
      , "values"
      , "(?, ?, ?, ?)"
      ] :: Query
