package telemetry

import (
	"github.com/jan-matejka/code-golf/message-queue/golang/src/core"
)

type Pusher interface {
	Push(core.SampleDesc, *core.Results)
}
