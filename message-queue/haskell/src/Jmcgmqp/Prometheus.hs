{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}
module Jmcgmqp.Prometheus
( cmdTestPrometheus
, newMetrics
, push
) where

import Prometheus (exportMetricsAsText, incGauge)

import Jmcgmqp.Runtime (Instance(runtime, metrics), Runtime)
import Jmcgmqp.Prometheus.Metrics (
  Metrics(test), newMetrics, SampleDesc(SampleDesc))

cmdTestPrometheus :: Instance -> IO ()
cmdTestPrometheus app = do
  putStrLn "Test prometheus"
  incGauge $ app.metrics.test
  push app.runtime (SampleDesc 4 "forkIO" "postgres") 3

push :: Runtime -> SampleDesc -> Int -> IO ()
push r sdesc wId = do
  samples <- exportMetricsAsText
  print wId
  print sdesc
  print r
  print samples
