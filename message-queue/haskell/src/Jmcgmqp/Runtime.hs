{-# LANGUAGE StandaloneKindSignatures, OverloadedRecordDot #-}
module Jmcgmqp.Runtime
( Instance(Instance, config, metrics, runtime)
, Runtime(..)
, newInstance
, newRuntime
, runtimeAsMap
) where

import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.Kind (Type)
import Data.Map (Map, fromList)
import Data.Maybe (fromJust)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Data.Time.LocalTime (getZonedTime, ZonedTime)
import Data.Time.RFC3339 (formatTimeRFC3339)
import Data.Version (showVersion)
import System.Info qualified as I
import Text.Printf (printf)

import Jmcgmqp.Config (newConfig, Config)
import Jmcgmqp.Uname (getKernel)
import Jmcgmqp.Prometheus.Metrics (newMetrics, Metrics)

type Runtime :: Type
data Runtime = Runtime {
  ctime :: ZonedTime,
  uuid :: UUID,
  lang :: String,
  lang_version :: String,
  runtime :: String,
  os :: String,
  kernel :: String,
  arch :: String
} deriving stock (Show)

newRuntime :: IO Runtime
newRuntime = do
  uuid_ <- fmap fromJust nextUUID
  kernel_ <- getKernel
  ctime_ <-  getZonedTime
  return $ Runtime {
    ctime = ctime_,
    uuid = uuid_,
    lang = "haskell",
    lang_version ="Haskell2010",
    runtime = runtime_,
    os = I.os,
    kernel = kernel_,
    arch = I.arch
    }
  where
    runtime_ :: String
    runtime_ = printf "%s %s" I.compilerName $ showVersion I.fullCompilerVersion

runtimeAsMap :: Runtime -> Map ByteString ByteString
runtimeAsMap r = fromList [
  ("ctime", pack $ formatTimeRFC3339 $ ctime r),
  ("uuid", pack $ show $ uuid r),
  ("lang", pack $ lang r),
  ("lang_version", pack $ lang_version r),
  ("runtime", pack $ r.runtime),
  ("os", pack $ os r),
  ("kernel", pack $ kernel r),
  ("arch", pack $ arch r)
  ]

type Instance :: Type
data Instance = Instance {
  config :: Config,
  runtime :: Runtime,
  metrics :: Metrics
  }

newInstance :: IO Instance
newInstance = do
  c <- newConfig
  r <- newRuntime
  Instance c r <$> newMetrics
