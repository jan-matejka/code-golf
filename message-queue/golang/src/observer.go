package jmcgmqp

type Observer func(Event, any)

type Publisher struct {
	subs map[Event][]Observer
}

func NewPublisher() *Publisher {
	return &Publisher{subs: make(map[Event][]Observer)}
}

func (p *Publisher) Register(e Event, sub Observer) {
	p.subs[e] = append(p.subs[e], sub)
}

// Notify sends an event update to subscribed observers with arbitrary data
func (p *Publisher) Notify(e Event, data any) {
	if observers, found := p.subs[e]; found {
		for _, observer := range observers {
			observer(e, data)
		}
	}
}
