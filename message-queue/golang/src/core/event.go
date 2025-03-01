package core

type Event int

const (
	SampleResults Event = iota + 1
	SamplingWorkers
)

func (e Event) String() string {
	return [...]string{"SampleResults", "SamplingWorkers"}[e-1]
}

func (e Event) EnumIndex() int {
	return int(e)
}
