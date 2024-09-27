module Jmcgmqp.Prometheus.Metrics
( newMetrics
, Metrics(test, messagesTotal, messagesPerSecond, durationSeconds)
, SampleDesc(..)
, sdescAsMap
) where

import Data.Kind (Type)
import Data.Map (fromList, Map)
import Data.ByteString.Lazy.Char8 (ByteString, pack)

import Prometheus (Gauge, gauge, register, Info(Info))

type Metrics :: Type
data Metrics = Metrics {
  test :: Gauge,
  messagesTotal :: Gauge,
  messagesPerSecond :: Gauge,
  durationSeconds :: Gauge
}

newMetrics :: IO Metrics
newMetrics = do
  t1 <- register $ gauge (Info "test" "Test Metric")
  t2 <- register $ gauge (Info "messages_total" "Messages sent.")
  t3 <- register $
    gauge (Info "messages_per_second" "Messages per second sent.")
  t4 <- register $ gauge (Info "duration_seconds" "Work duration in seconds.")
  return $ Metrics t1 t2 t3 t4

type SampleDesc :: Type
data SampleDesc = SampleDesc {
  nWorkers :: Int,
  algorithm :: String,
  mqSystem :: String
  } deriving stock (Show)

sdescAsMap :: SampleDesc -> Map ByteString ByteString
sdescAsMap s = fromList [
  ("n_workers", pack $ show s.nWorkers),
  ("algorithm", pack s.algorithm),
  ("mq_system", pack s.mqSystem)
  ]
