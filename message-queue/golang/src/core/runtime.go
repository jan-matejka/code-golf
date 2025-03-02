package core

import (
	"github.com/bensallen/modscan/pkg/uname"
	"github.com/google/uuid"
	"runtime"
	"time"
)

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
