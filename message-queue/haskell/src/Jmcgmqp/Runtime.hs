{-# LANGUAGE Safe #-}
module Jmcgmqp.Runtime
( Instance(Instance)
, newInstance
, config
) where

import Data.Functor ((<&>))
import Jmcgmqp.Config (newConfig, Config)

newtype Instance = Instance {
  config :: Config
  }

newInstance :: IO Instance
newInstance = newConfig <&> Instance
