package jmcgmqp

import (
	"fmt"
	"os"
)

func Die(message string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, message, args...)
	os.Exit(1)
}
