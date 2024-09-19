package golang

import "os"
import "strconv"

type Config struct {
	Test_prometheus int
}

func NewConfig() (*Config, error) {
	c := new(Config)

	x, exists := os.LookupEnv("TEST_PROMETHEUS")
	if exists {
		i, err := strconv.Atoi(x)
		if err != nil {
			return nil, err
		}
		c.Test_prometheus = i
	}

	return c, nil
}
