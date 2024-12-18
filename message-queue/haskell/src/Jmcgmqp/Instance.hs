module Jmcgmqp.Instance
( Instance(Instance, config, metrics, runtime, pg)
, newInstance
) where

import Data.Kind (Type)

import Jmcgmqp.Config (newConfig, Config)
import Jmcgmqp.Prometheus.Metrics (newMetrics, Metrics)
import Jmcgmqp.Postgres (newPostgres, Postgres)
import Jmcgmqp.Runtime (Runtime, newRuntime)

type Instance :: Type
data Instance = Instance {
  config :: Config,
  runtime :: Runtime,
  pg :: Postgres,
  metrics :: Metrics
  }

newInstance :: IO Instance
newInstance = do
  c <- newConfig
  r <- newRuntime
  pg <- newPostgres "postgres://postgres@localhost:5432/mq"
  Instance c r pg <$> newMetrics
