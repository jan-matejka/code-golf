{-# LANGUAGE StandaloneKindSignatures #-}
module Jmcgmqp.Runtime
( Instance(Instance)
, newInstance
, config
, newRuntime
, runtimeAsMap
) where

import Data.Kind (Type)
import Data.Functor ((<&>))
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

runtimeAsMap :: Runtime -> Map String String
runtimeAsMap r = fromList [
  ("ctime", formatTimeRFC3339 $ ctime r),
  ("uuid", show $ uuid r),
  ("lang", lang r),
  ("lang_version", lang_version r),
  ("runtime", runtime r),
  ("os", os r),
  ("kernel", kernel r),
  ("arch", arch r)
  ]

type Instance :: Type
newtype Instance = Instance {
  config :: Config
  }

newInstance :: IO Instance
newInstance = newConfig <&> Instance
