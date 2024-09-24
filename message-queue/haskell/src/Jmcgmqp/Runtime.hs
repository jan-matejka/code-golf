{-# LANGUAGE Safe #-}
module Jmcgmqp.Runtime
( Instance(Instance)
, newInstance
, config
) where

import Jmcgmqp.Config (newConfig, Config)

data Instance = Instance {
  config :: Config
  }

newInstance :: IO (Instance)
newInstance = newConfig >>= return . Instance
