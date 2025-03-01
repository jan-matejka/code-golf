package core

import "os"
import "strconv"

type Config struct {
	Test_prometheus   int
	Duration          int
	TelemetryPostgres string
}

func DefaultConfig() *Config {
	c := new(Config)
	c.Test_prometheus = 0
	c.Duration = 3
	c.TelemetryPostgres = "postgres://mq@localhost:5442/mq"
	return c
}

func NewConfig() (*Config, error) {
	c := DefaultConfig()

	{
		x, exists := os.LookupEnv("TEST_PROMETHEUS")
		if exists {
			i, err := strconv.Atoi(x)
			if err != nil {
				return nil, err
			}
			c.Test_prometheus = i
		}
	}

	{
		x, exists := os.LookupEnv("DURATION")
		if exists {
			i, err := strconv.Atoi(x)
			if err != nil {
				return nil, err
			}
			c.Duration = i
		}
	}

	{
		x, exists := os.LookupEnv("TELEMETRY_POSTGRES")
		if exists {
			c.TelemetryPostgres = x
		}
	}

	return c, nil
}
