{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}
module Jmcgmqp.Prometheus
( cmdTestPrometheus
, newMetrics
, push
) where

import Prometheus (exportMetricsAsText, incGauge)

import Jmcgmqp.Runtime (Instance(runtime, metrics))
import Jmcgmqp.Prometheus.Metrics (Metrics(test), newMetrics)

cmdTestPrometheus :: Instance -> IO ()
cmdTestPrometheus app = do
  putStrLn "Test prometheus"
  print $ app.runtime
  incGauge $ app.metrics.test
  push

push :: IO ()
push = do
  samples <- exportMetricsAsText
  print samples
