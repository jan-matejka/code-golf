package golang

import "testing"

func TestMessagesPerSecond(t *testing.T) {
	rs := NewResults()
	rs.Add(NewWorkerResult(1, 10, 1_000_000_000))
	rs.Add(NewWorkerResult(2, 20, 2_000_000_000))
	if rs.MessagesPerSecond != 10 {
		t.Fatalf("Expected %v, got %v", 10, rs.MessagesPerSecond)
	}
}
