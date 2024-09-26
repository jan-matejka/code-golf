{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}
module Jmcgmqp.Prometheus
( cmdTestPrometheus
, newMetrics
, push
) where

import Data.Map (Map, fromList, union, toList)
import Data.ByteString.Lazy.Char8 qualified as B

import Prometheus (exportMetricsAsText, incGauge)

import Jmcgmqp.Runtime (Instance(runtime, metrics), Runtime, runtimeAsMap)
import Jmcgmqp.Prometheus.Metrics (
  Metrics(test), newMetrics, SampleDesc(SampleDesc), sdescAsMap)

cmdTestPrometheus :: Instance -> IO ()
cmdTestPrometheus app = do
  putStrLn "Test prometheus"
  incGauge $ app.metrics.test
  push app.runtime (SampleDesc 4 "forkIO" "postgres") 3

push :: Runtime -> SampleDesc -> Int -> IO ()
push r sdesc wId = do
  samples <- exportMetricsAsText
  B.putStrLn $ applyLabels labels samples
  where
    labels :: B.ByteString
    labels = packLabels $ union
      (runtimeAsMap r)
      $ union
        (sdescAsMap sdesc)
        (fromList [("worker_id", B.pack $ show wId)])

packLabels :: Map B.ByteString B.ByteString -> B.ByteString
packLabels m = wrap (join (map pack $ toList m))
  where
    wrap :: B.ByteString -> B.ByteString
    wrap s = "{" `B.append` s `B.append` "}"
    join :: [B.ByteString] -> B.ByteString
    join = B.intercalate ","
    pack :: (B.ByteString, B.ByteString) -> B.ByteString
    pack (k, v) = k `B.append` "=\"" `B.append` v `B.append` "\""

-- | apply labels to exported unlabeled metrics
-- TBD: Vulnerable to injection
applyLabels :: B.ByteString -> B.ByteString -> B.ByteString
applyLabels labels b = B.unlines $  [
  if i `mod` (3::Int) == 0 then apply line
  else line
  | (i, line) <- zip [1..] $ B.lines b
  ]
  where
    apply :: B.ByteString -> B.ByteString
    apply line = metric `B.append`labels `B.append` " " `B.append` value
      where
        metric :: B.ByteString
        metric = head $ B.words line
        value :: B.ByteString
        value = last $ B.words line
