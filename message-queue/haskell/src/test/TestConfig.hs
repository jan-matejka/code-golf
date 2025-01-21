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
, telemetryPgBase :: String
, telemetryPgRoot :: B.ByteString
, telemetryPgMq :: B.ByteString
}

rootDSN :: String -> B.ByteString
rootDSN = B8.pack . printf "postgres://postgres@%s"

mqDSN :: String -> B.ByteString
mqDSN = B8.pack . printf "postgres://mq@%s/test"

-- TBD: look into memoizing this.
-- https://hspec.github.io/hspec-discover.html
newTestConfig :: IO TestConfig
newTestConfig = do
  mq_pg <- sgetenv "PG_TEST_DSN" "localhost:5433"
  telemetry_pg <- sgetenv "TEST_TELEMETRY_POSTGRES" "localhost:5443"
  return TestConfig {
    pg_test_dsn = mq_pg
  , pg_test_root_dsn = rootDSN mq_pg
  , pg_test_mq_dsn = mqDSN mq_pg
  , telemetryPgBase = telemetry_pg
  , telemetryPgRoot = rootDSN telemetry_pg
  , telemetryPgMq = mqDSN telemetry_pg
  }
