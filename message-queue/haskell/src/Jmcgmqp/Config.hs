{-# LANGUAGE StandaloneKindSignatures #-}
module Jmcgmqp.Config
( Config(Config)
, test_prometheus
, duration
, newConfig
) where

import System.Environment (lookupEnv)
import Data.Kind (Type)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

type Config :: Type
data Config = Config {
    test_prometheus :: Int,
    duration :: Int
  }

igetenv :: String -> String -> IO String
igetenv var def = lookupEnv var <&> fromMaybe def

newConfig :: IO Config
newConfig = do
  tp <- igetenv "TEST_PROMETHEUS" "0"
  dur <- igetenv "DURATION" "3"
  return $ Config {
    test_prometheus = read tp,
    duration = read dur
  }
