module Jmcgmqp.Worker
( newResults
, newWorkerResult
, Results(..)
, insert
, setMetrics
, WorkerResult(..)
) where

import Data.Kind (Type)
import Database.PostgreSQL.Simple (
  Connection, execute, Only(Only))
import Control.Monad (void)
import System.Clock (TimeSpec, toNanoSecs)

import Prometheus (setGauge)

import Jmcgmqp.Prometheus.Metrics (
  Metrics(messagesTotal, messagesPerSecond, durationSeconds)
  )

insert :: Connection -> Int -> IO ()
insert conn =
  void . execute conn "insert into public.queue (data) values (?)" . Only . show

type WorkerResult :: Type
data WorkerResult = WorkerResult {
  workerId :: Int,
  messagesTotal :: Int,
  duration :: TimeSpec,
  messagesPerSecond :: Double
} deriving stock (Show)

newWorkerResult :: Int -> Int -> TimeSpec -> WorkerResult
newWorkerResult wId mTotal dur = WorkerResult {
  workerId = wId,
  messagesTotal = mTotal,
  duration = dur,
  messagesPerSecond = mps mTotal dur
  }

mps :: Int -> TimeSpec -> Double
mps n d = fromIntegral n / durSecs d :: Double

durSecs :: TimeSpec -> Double
durSecs d = (fromIntegral $ toNanoSecs d) * (10::Double) ^^ (-9::Int)

type Results :: Type
data Results = Results {
  workers :: [WorkerResult],
  messagesTotal :: Int,
  duration :: TimeSpec,
  messagesPerSecond :: Double
} deriving stock (Show)

newResults :: [WorkerResult] -> Results
newResults ws = Results {
  workers = ws,
  messagesTotal = totalMsg,
  duration = totalDur,
  messagesPerSecond = mps ((length ws) * totalMsg) totalDur
  }
  where
    totalMsg :: Int
    totalMsg = sum $ map (\WorkerResult{messagesTotal=n} -> n) ws
    totalDur :: TimeSpec
    totalDur = sum $ map (\WorkerResult{duration=d} -> d) ws

setMetrics :: Metrics -> WorkerResult -> IO ()
setMetrics m wr = do
  setGauge m.messagesTotal $ fromIntegral wr.messagesTotal
  setGauge m.messagesPerSecond wr.messagesPerSecond
  setGauge m.durationSeconds $ durSecs wr.duration
  return ()
