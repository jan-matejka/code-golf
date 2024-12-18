module Jmcgmqp.Postgres (
  Postgres(..)
, newPostgres
, push
) where

import Control.Monad (void)
import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only), connectPostgreSQL, query, Query,
  query_)
import Data.ByteString (ByteString)
import System.Clock (toNanoSecs)

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
  [Only runtime_id] <- query_ pg.conn q
  return runtime_id
  where
    q :: Query
    q = "\
      \ insert into results.runtime ( \
      \  ctime, uuid, lang, lang_version, runtime, os, kernel, arch \
      \ ) values ( \
      \ '2010-10-10 1:2:3', '', '', '', '', '', '', '' \
      \ ) \
      \ returning id; \
      \ "

    args = (
      "0"::String,
      ""::String,
      "lang"::String,
      "lver"::String,
      "runtime"::String,
      "os"::String,
      "ernel"::String,
      "arch"::String
      )

    {- ( r.ctime
    , r.uuid
    , r.lang
    , r.lang_version
    , r.runtime
    , r.os
    , r.kernel
    , r.arch
    )-}

createSample :: Postgres -> Int -> SampleDesc -> IO Int
createSample pg runtime_id sdesc = do
  let q = "\
        \ with \
        \ sel as ( \
        \   select id from results.sample where \
        \       runtime_id = ? \
        \       and n_workers = ? \
        \       and algorithm = ? \
        \       and mq_system = ? \
        \ ), \
        \ ins as ( \
        \   insert into results.sample \
        \   (runtime_id, n_workers, algorithm, mq_system) \
        \   values \
        \   (?, ?, ?, ?) \
        \   on conflict do nothing \
        \   returning id \
        \ ) \
        \ select * from ins \
        \ union \
        \ select * from sel \
        \ where id is not null; \
        \ "
  [Only sample_id]
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
  return sample_id

createWorker :: Postgres -> Int -> WorkerResult -> IO ()
createWorker pg sample_id w =
  void $ execute pg.conn q
    (sample_id, w.workerId, w.messagesTotal, (toNanoSecs w.duration))
  where
    q = " \
      \ insert into results.worker \
      \ (sample_id, worker_id, messages_total, duration_ns) \
      \ values \
      \ (?, ?, ?, ?) \
      \ " :: Query
