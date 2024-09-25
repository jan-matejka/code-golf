{-# LANGUAGE Safe, StandaloneKindSignatures #-}
module Jmcgmqp.Runtime
( Instance(Instance)
, newInstance
, config
) where

import Data.Kind (Type)
import Data.Functor ((<&>))

import Jmcgmqp.Config (newConfig, Config)

type Instance :: Type
newtype Instance = Instance {
  config :: Config
  }

newInstance :: IO Instance
newInstance = newConfig <&> Instance
