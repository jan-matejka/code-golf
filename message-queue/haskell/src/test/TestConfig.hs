module TestConfig
( TestConfig(..)
, newTestConfig
) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.Kind (Type)
import Text.Printf (printf)
import Jmcgmqp.Config (sgetenv)

type TestConfig :: Type
data TestConfig = TestConfig {
  pg_test_dsn :: String
, pg_test_root_dsn :: B.ByteString
, pg_test_mq_dsn :: B.ByteString
}

-- TBD: look into memoizing this.
-- https://hspec.github.io/hspec-discover.html
newTestConfig :: IO TestConfig
newTestConfig = do
  base <- sgetenv "PG_TEST_DSN" "localhost:5433"
  return TestConfig {
    pg_test_dsn = base
  , pg_test_root_dsn = B8.pack $ printf "postgres://postgres@%s" base
  , pg_test_mq_dsn = B8.pack $ printf "postgres://mq@%s/test" base
  }
