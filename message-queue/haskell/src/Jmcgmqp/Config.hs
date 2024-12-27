module Jmcgmqp.Config
( Config(Config)
, test_prometheus
, duration
, power
, newConfig
, sgetenv
) where

import System.Environment (lookupEnv)
import Data.Kind (Type)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

type Config :: Type
data Config = Config {
    test_prometheus :: Int,
    duration :: Int,
    power :: Int
  }

igetenv :: String -> String -> IO String
igetenv var def = lookupEnv var <&> fromMaybe def

sgetenv :: String -> String -> IO String
sgetenv var def = lookupEnv var <&> fromMaybe def

newConfig :: IO Config
newConfig = do
  tp <- igetenv "TEST_PROMETHEUS" "0"
  dur <- igetenv "DURATION" "3"
  pow <- igetenv "POWER" "0"
  return $ Config {
    test_prometheus = read tp,
    duration = read dur,
    power = read pow
  }
