{-# LANGUAGE StandaloneKindSignatures #-}
module Jmcgmqp.Config
( Config
, test_prometheus
, newConfig
) where

import System.Environment (lookupEnv)
import Data.Kind (Type)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

type Config :: Type
newtype Config = Config {
    test_prometheus :: Int
  }

igetenv :: String -> String -> IO String
igetenv var def = lookupEnv var <&> fromMaybe def

newConfig :: IO Config
newConfig = do
  tp <- igetenv "TEST_PROMETHEUS" "0"
  return $ Config $ read tp
