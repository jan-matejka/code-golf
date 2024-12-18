module Jmcgmqp.Prometheus
( cmdTestPrometheus
, newMetrics
, push
) where

import Data.Map (Map, fromList, union)
import Data.ByteString.Lazy.Char8 qualified as B

import Prometheus (exportMetricsAsText, incGauge)

import Jmcgmqp.Instance (Instance(runtime, metrics))
import Jmcgmqp.Runtime (Runtime, runtimeAsMap)
import Jmcgmqp.Prometheus.Metrics (
  Metrics(test), newMetrics, SampleDesc(SampleDesc), sdescAsMap)
import Jmcgmqp.Prometheus.Http (pushAdd)

cmdTestPrometheus :: Instance -> IO ()
cmdTestPrometheus app = do
  putStrLn "Test prometheus"
  incGauge $ app.metrics.test
  push app.runtime (SampleDesc 4 "forkIO" "postgres") 3

push :: Runtime -> SampleDesc -> Int -> IO ()
push r sdesc wId = do
  samples <- exportMetricsAsText
  -- pushAdd labelMap $ applyLabels labels samples
  pushAdd labelMap samples
  where
    labelMap :: Map B.ByteString B.ByteString
    labelMap = union
      (runtimeAsMap r)
      $ union
        (sdescAsMap sdesc)
        (fromList [("worker_id", B.pack $ show wId)])

-- -- | Use as::
-- -- pushAdd labelMap $ applyLabels (packLabels labelMap) samples
-- --  turns out I need to put them into labels,not body for "push add"
-- 
-- packLabels :: Map B.ByteString B.ByteString -> B.ByteString
-- packLabels m = wrap (join (map pack $ toList m))
--   where
--     wrap :: B.ByteString -> B.ByteString
--     wrap s = "{" `B.append` s `B.append` "}"
--     join :: [B.ByteString] -> B.ByteString
--     join = B.intercalate ","
--     pack :: (B.ByteString, B.ByteString) -> B.ByteString
--     pack (k, v) = k `B.append` "=\"" `B.append` v `B.append` "\""
-- 
-- -- | apply labels to exported unlabeled metrics
-- -- Note: vulnerable to injection, use URI.encode or smth
-- applyLabels :: B.ByteString -> B.ByteString -> B.ByteString
-- applyLabels labels b = B.unlines $  [
--   if i `mod` (3::Int) == 0 then apply line
--   else line
--   | (i, line) <- zip [1..] $ B.lines b
--   ]
--   where
--     apply :: B.ByteString -> B.ByteString
--     apply line = metric `B.append`labels `B.append` " " `B.append` value
--       where
--         metric :: B.ByteString
--         metric = head $ B.words line
--         value :: B.ByteString
--         value = last $ B.words line
