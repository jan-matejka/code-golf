module PostgresSpec
( spec )
where

import Test.Hspec (Spec, describe, it, shouldBe)

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.State (execStateT)
import System.Clock (TimeSpec(TimeSpec), fromNanoSecs)
import Data.ByteString (ByteString)
import Data.Time.LocalTime (LocalTime, ZonedTime(zonedTimeToLocalTime))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (execute_, connectPostgreSQL,
  query_, Only, Connection, Query)
import Database.PostgreSQL.Simple qualified as PSQL (close)

import Jmcgmqp.Prometheus.Metrics (
  SampleDesc(SampleDesc, mqSystem, algorithm, nWorkers))
import Jmcgmqp.Worker (newWorkerResult, newResults,
  Results(workers),
  WorkerResult(duration, messagesTotal, workerId),
  )
import Jmcgmqp.Postgres (newPostgres, push, Postgres(conn), close)
import Jmcgmqp.Runtime (newRuntime,
  Runtime(ctime, uuid, lang, lang_version, runtime, os, kernel, arch)
  )
import TestConfig (newTestConfig,
  TestConfig(telemetryPgMq, telemetryPgRoot)
  )

withConnect :: ByteString -> (Connection -> IO c) -> IO c
withConnect x = bracket (connectPostgreSQL x) PSQL.close

withConnect2 :: ByteString -> (Postgres -> IO c) -> IO c
withConnect2 x = bracket (newPostgres x) close

mkDb :: IO ()
mkDb = do
  tcg <- newTestConfig
  withConnect tcg.telemetryPgRoot
   (\c ->  void (
      execute_ c "drop database if exists test" >>
      execute_ c "create database test template mq"
      ))

test_runtime :: Postgres -> Runtime -> IO Int
test_runtime pg r = do
  [(
    runtime_id
    , ctime
    , uuid
    , lang
    , lang_version
    , runtime
    , os
    , kernel
    , arch
    )] <- query_ pg.conn q

  (runtime_id::Int) `shouldBe` 1
  -- Notes on LocalTime:
  -- - Inserting LocalTime into postgres results into record with the
  --   time rounded mathematically to micros.
  -- - However, formatTime to %6Q simply truncates to micros.
  -- - For this reason we insert LocalTime into postgres already truncated as
  --   the time package does not support rounding.
  -- - We can not round manually (Decimal) either because we would need
  --   to carry the overflow trough all the datetime parts.
  -- - Also worth nothing is that one needs to use padding modifiers
  --   in order to get parts like ":09" instead of ":9" and the
  --   unpadded version can not be read by `Read LocalTime` instance.
  -- - Hypothetically there could be conversion to UNIX timestamp,
  --   rounding, then conversion back that would solve it most cleanly
  --   I suppose but I just don't care anymore.

  (ctime::LocalTime)
    `shouldBe` (
      read $
        formatTime
          defaultTimeLocale
          "%0F %0T%6Q"
          r.ctime.zonedTimeToLocalTime :: LocalTime
    )
  (read uuid::UUID) `shouldBe` r.uuid
  (lang::String) `shouldBe` r.lang
  (lang_version::String) `shouldBe` r.lang_version
  (runtime::String) `shouldBe` r.runtime
  (os::String) `shouldBe` r.os
  (kernel::String) `shouldBe` r.kernel
  (arch::String) `shouldBe` r.arch
  return runtime_id

  where
    q :: Query
    q = "\
        \ select \
        \  id,ctime,uuid,lang,lang_version,runtime,os,kernel,arch \
        \ from results.runtime \
        \ "

test_samples :: Postgres -> Int -> SampleDesc -> IO Int
test_samples pg in_runtime_id sdesc = do
  [(
    sample_id,
    runtime_id,
    n_workers,
    algorithm,
    mq_system
    )] <- query_ pg.conn q

  (sample_id::Int) `shouldBe` 1
  (runtime_id::Int) `shouldBe` in_runtime_id
  (n_workers::Int) `shouldBe` sdesc.nWorkers
  (algorithm::String) `shouldBe` sdesc.algorithm
  (mq_system::String) `shouldBe` sdesc.mqSystem

  return sample_id
  where
    q :: Query
    q = " \
    \ select \
    \     id,runtime_id,n_workers,algorithm,mq_system \
    \ from results.sample \
    \ ";

test_workers :: Postgres -> Int -> Results -> IO ()
test_workers pg in_sample_id rs = do
    [w1, w2] <- query_ pg.conn q

    _ <- check 1 w1 (rs.workers !! 0)
    _ <- check 2 w2 (rs.workers !! 1)
    return ()
  where
    q :: Query
    q = " \
    \ select \
    \     id,sample_id,worker_id,messages_total,duration_ns \
    \ from results.worker \
    \ ";

    check :: Int -> (Int, Int, Int, Int, Integer) -> WorkerResult -> IO ()
    check in_id (id, sample_id, worker_id, messages_total, duration_ns) wr = do
      id `shouldBe` in_id
      sample_id `shouldBe` in_sample_id
      worker_id `shouldBe` wr.workerId
      messages_total `shouldBe` wr.messagesTotal
      (fromNanoSecs duration_ns) `shouldBe` wr.duration

spec :: Spec
spec = do
  describe "Postgres.push" $ do
    it "creates runtime, samples, and workers" $ do
      let sdesc = SampleDesc 2 "forkIO" "postgres"
      let rs = newResults [
               newWorkerResult 1 10 $ TimeSpec 1 0
             , newWorkerResult 2 20 $ TimeSpec 2 0
             ]
      _ <- mkDb
      tcg <- newTestConfig
      r <- newRuntime
      withConnect2 tcg.telemetryPgMq $ \pg ->
        void $ execStateT (push pg r sdesc rs) Nothing

      withConnect2 tcg.telemetryPgMq $ \pg -> do
        runtime_id <- test_runtime pg r
        sample_id <- test_samples pg runtime_id sdesc
        test_workers pg sample_id rs
        return ()

    it "retains runtime id" $ do
      let sdesc1 = SampleDesc 2 "forkIO" "postgres"
          sdesc2 = SampleDesc 3 "forkIO" "postgres"
      let rs = newResults [
               newWorkerResult 1 10 $ TimeSpec 1 0
             , newWorkerResult 2 20 $ TimeSpec 2 0
             ]
      _ <- mkDb
      tcg <- newTestConfig
      withConnect2 tcg.telemetryPgMq $ \pg -> do
          r <- newRuntime

          _ <- execStateT (push pg r sdesc1 rs >> push pg r sdesc2 rs) Nothing
          xs <- query_ pg.conn "select id from results.runtime"
          length (xs::[Only Int])  `shouldBe` 1
