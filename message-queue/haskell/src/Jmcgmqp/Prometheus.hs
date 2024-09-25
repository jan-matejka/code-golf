{-# LANGUAGE OverloadedStrings, ImportQualifiedPost #-}
module Jmcgmqp.Prometheus
( cmdTestPrometheus
, newMetrics
) where

import Prometheus qualified as P

newMetrics :: IO [P.Gauge]
newMetrics = do
  test <- P.register $ P.gauge (P.Info "test" "Test Metric")
  return [test]

cmdTestPrometheus :: IO ()
cmdTestPrometheus = do
  putStrLn "Test prometheus"
