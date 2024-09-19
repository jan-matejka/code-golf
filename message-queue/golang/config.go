package golang

import "os"
import "strconv"

type Config struct {
	Test_prometheus int
	Duration        int
}

func DefaultConfig() *Config {
	c := new(Config)
	c.Test_prometheus = 0
	c.Duration = 3
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

	return c, nil
}
