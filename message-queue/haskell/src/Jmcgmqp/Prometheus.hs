{-# LANGUAGE OverloadedStrings #-}
module Jmcgmqp.Prometheus
( cmdTestPrometheus
, newMetrics
, push
) where

import Prometheus (
  Gauge, gauge, register, Info(Info),
  exportMetricsAsText, incGauge
  )

import Jmcgmqp.Runtime (newRuntime)

newMetrics :: IO [Gauge]
newMetrics = do
  test <- register $ gauge (Info "test" "Test Metric")
  return [test]

cmdTestPrometheus :: IO ()
cmdTestPrometheus = do
  putStrLn "Test prometheus"
  r <- newRuntime
  print r
  m <- newMetrics
  incGauge $ head m
  push

push :: IO ()
push = do
  samples <- exportMetricsAsText
  print samples
