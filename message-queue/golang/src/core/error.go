package core

type FailedToCastError struct {
	Data any
	To   string
}
