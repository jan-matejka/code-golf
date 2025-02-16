package jmcgmqp

import "runtime"
import "time"
import "github.com/google/uuid"
import "github.com/prometheus/client_golang/prometheus/push"
import "github.com/bensallen/modscan/pkg/uname"

type Instance struct {
	Runtime    *Runtime
	Config     *Config
	Prometheus *push.Pusher
}

func NewInstance() (*Instance, error) {
	runtime, err := NewRuntime()
	if err != nil {
		return nil, err
	}
	i := new(Instance)
	i.Runtime = runtime
	c, err := NewConfig()
	if err != nil {
		return nil, err
	}
	i.Config = c

	i.Prometheus = NewPusher()
	return i, nil
}

type Runtime struct {
	Ctime        time.Time
	Uuid         uuid.UUID
	Lang         string
	Lang_version string
	Runtime      string
	Os           string
	Kernel       string
	Arch         string
}

func NewRuntime() (*Runtime, error) {
	uuid, err := uuid.NewUUID()
	if err != nil {
		return nil, err
	}

	uname, err := uname.New()
	if err != nil {
		return nil, err
	}

	r := &Runtime{
		time.Now(),
		uuid,
		"golang",
		runtime.Version(),
		"golang",
		runtime.GOOS,
		uname.Release(),
		runtime.GOARCH,
	}

	return r, nil
}

func (r Runtime) Map() map[string]string {
	return map[string]string{
		"ctime":        r.Ctime.Format(time.RFC3339),
		"uuid":         r.Uuid.String(),
		"lang":         r.Lang,
		"lang_version": r.Lang_version,
		"runtime":      r.Runtime,
		"os":           r.Os,
		"kernel":       r.Kernel,
		"arch":         r.Arch,
	}
}

func RuntimeFieldNames() []string {
	return []string{"ctime", "uuid", "lang", "lang_version", "runtime", "os", "kernel", "arch"}
}
