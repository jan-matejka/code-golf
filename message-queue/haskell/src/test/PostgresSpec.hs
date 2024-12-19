module PostgresSpec
( spec )
where

import Test.Hspec
import Test.QuickCheck

import System.Clock (TimeSpec(..))
import Control.Monad (void)

import Database.PostgreSQL.Simple (execute, connectPostgreSQL)

import Jmcgmqp.Prometheus.Metrics (SampleDesc(SampleDesc))
import Jmcgmqp.Worker
import Jmcgmqp.Postgres

mkDb :: IO ()
mkDb = do
  c <- connectPostgreSQL "postgres://postgres@localhost:5433"
  void . execute c "drop database if exists test"
  void . execute c "create database test template mq"

spec :: Spec
spec = do
  describe "Postgres.push" $ do
    it "works" $ do
      let sdesc = SampleDesc 2 "forkIO" "postgres"
      let rs = newResults [
               newWorkerResult 1 10 $ TimeSpec 1 0
             , newWorkerResult 2 20 $ TimeSpec 2 0
             ]
      mkDb
      pg <- newPostgres "postgres://postgres@localhost:5433/test"
      r <- newRuntime
      push pg r sdesc rs
      0 `shouldBe` 1
