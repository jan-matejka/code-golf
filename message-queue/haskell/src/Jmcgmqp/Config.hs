module Jmcgmqp.Config
( Config
, test_prometheus
, newConfig
) where

import System.Environment
import Data.Maybe

data Config = Config {
  test_prometheus :: Int
}

igetenv :: String -> String -> IO String
igetenv var def = lookupEnv var >>= return . fromMaybe def

newConfig :: IO Config
newConfig = do
  tp <- igetenv "TEST_PROMETHEUS" "0"
  return $ Config $ read tp
