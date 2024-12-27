package test

import (
	"fmt"
	"os"
)

type testConfig struct {
	PgTestRootDSN string
	PgTestMqDSN   string
}

var TestConfig *testConfig

func defaultTestConfig() *testConfig {
	c := new(testConfig)
	pg, exists := os.LookupEnv("PG_TEST_DSN")
	if !exists {
		pg = "localhost:5433"
	}
	c.PgTestRootDSN = fmt.Sprintf("postgres://postgres@%s", pg)
	c.PgTestMqDSN = fmt.Sprintf("postgres://mq@%s/test", pg)
	return c
}

func init() {
	TestConfig = defaultTestConfig()
}
