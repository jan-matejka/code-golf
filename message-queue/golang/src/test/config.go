package test

import (
	"fmt"
	"os"
)

type testConfig struct {
	PgTestRootDSN         string
	PgTestMqDSN           string
	TelemetryPostgresBase string
	TelemetryPostgresRoot string
	TelemetryPostgresMq   string
}

var TestConfig *testConfig

func fmtBase(base string) (string, string) {
	root := fmt.Sprintf("postgres://postgres@%s", base)
	user := fmt.Sprintf("postgres://mq@%s/test", base)
	return root, user
}

func (c *testConfig) SetPgTestBase(base string) {
	c.PgTestRootDSN, c.PgTestMqDSN = fmtBase(base)
}

func (c *testConfig) SetTelemetryPostgresBase(base string) {
	c.TelemetryPostgresBase = base
	c.TelemetryPostgresRoot, c.TelemetryPostgresMq = fmtBase(base)
}

func defaultTestConfig() *testConfig {
	c := new(testConfig)
	c.SetPgTestBase("localhost:5433")
	c.SetTelemetryPostgresBase("localhost:5443")
	return c
}
func newTestConfig() *testConfig {
	c := defaultTestConfig()

	x, exists := os.LookupEnv("PG_TEST_DSN")
	if exists {
		c.SetPgTestBase(x)
	}

	x, ok := os.LookupEnv("TEST_TELEMETRY_POSTGRES")
	if ok {
		c.SetTelemetryPostgresBase(x)
	}

	return c
}

func init() {
	TestConfig = newTestConfig()
}
