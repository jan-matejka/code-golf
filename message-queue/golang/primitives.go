package golang

import "time"
import "math"

type WorkerResult struct {
	WorkerId      int
	MessagesTotal int
	DurationNs    time.Duration

	MessagesPerSecond float64
	DurationSeconds   float64
}

func NewWorkerResult(workerId int, messagesTotal int, durationNs time.Duration) *WorkerResult {
	r := new(WorkerResult)

	r.WorkerId = workerId
	r.MessagesTotal = messagesTotal
	r.DurationNs = durationNs

	r.DurationSeconds = float64(durationNs) * math.Pow10(-9)
	r.MessagesPerSecond = float64(messagesTotal) / r.DurationSeconds

	return r
}
