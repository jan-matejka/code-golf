module Jmcgmqp.Postgres (
  Postgres(..)
, newPostgres
, push
, PushMonad
, withPostgres
, close
) where

import Control.Monad (void)
import Control.Monad.State (StateT, evalStateT, liftIO, get, put)
import Data.Kind (Type)
import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only), connectPostgreSQL, query, Query)
import Database.PostgreSQL.Simple qualified as PSQL (close)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import System.Clock (toNanoSecs)
import Data.Time.LocalTime (ZonedTime(zonedTimeToLocalTime))
import Data.Time.Format (formatTime, defaultTimeLocale)

import Jmcgmqp.Runtime (
  Runtime(ctime, uuid, lang, lang_version, runtime, os, kernel, arch))
import Jmcgmqp.Prometheus.Metrics (SampleDesc(nWorkers, algorithm, mqSystem))
import Jmcgmqp.Worker (Results(workers),
  WorkerResult(workerId, messagesTotal, duration))

type Postgres :: Type
newtype Postgres = Postgres {
  conn :: Connection
}

newPostgres :: ByteString -> IO Postgres
newPostgres x = do
  c <- connectPostgreSQL x
  return $ Postgres c

close :: Postgres -> IO ()
close = PSQL.close . conn

-- Remember runtime_id
type PushState :: Type
type PushState = Maybe Int

type PushMonad :: Type -> Type
type PushMonad a = StateT PushState IO a

createRuntimeIfMissing :: Postgres -> Runtime -> PushMonad Int
createRuntimeIfMissing pg r = do
  s <- get
  case s of
    Nothing -> liftIO (createRuntime pg r) >>= put . Just >> fromJust <$> get
    Just id -> return id

push :: Postgres -> Runtime -> SampleDesc -> Results -> PushMonad ()
push pg r sdesc rs = do
  runtime_id <- createRuntimeIfMissing pg r
  sample_id <- liftIO $ createSample pg runtime_id sdesc
  liftIO $ mapM_ (createWorker pg sample_id) (rs.workers::[WorkerResult])

withPostgres :: PushMonad a -> IO a
withPostgres f = evalStateT f Nothing

createRuntime :: Postgres -> Runtime -> IO Int
createRuntime pg r = do
  [Only runtime_id] <- query pg.conn q args
  return runtime_id
  where
    q :: Query
    q = "\
      \ insert into results.runtime ( \
      \  ctime, uuid, lang, lang_version, runtime, os, kernel, arch \
      \ ) values ( \
      \   ?, ?, ?, ?, ?, ?, ?, ? \
      \ ) \
      \ returning id; \
      \ "

    args =
      ( formatTime defaultTimeLocale "%0F %0T%6Q" r.ctime.zonedTimeToLocalTime
      , show r.uuid
      , r.lang
      , r.lang_version
      , r.runtime
      , r.os
      , r.kernel
      , r.arch
      )

createSample :: Postgres -> Int -> SampleDesc -> IO Int
createSample pg runtime_id sdesc = do
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
  where
    q :: Query
    q = "\
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
