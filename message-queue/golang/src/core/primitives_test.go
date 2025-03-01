package core

import "testing"

func TestMessagesPerSecond(t *testing.T) {
	rs := NewResults()
	rs.Add(NewWorkerResult(1, 10, 1_000_000_000))
	rs.Add(NewWorkerResult(2, 10, 1_000_000_000))
	if rs.MessagesPerSecond != 20 {
		t.Fatalf("Expected %v, got %v", 20, rs.MessagesPerSecond)
	}

	rs = NewResults()
	rs.Add(NewWorkerResult(1, 10, 1_000_000_000))
	rs.Add(NewWorkerResult(2, 10, 1_000_000_000))
	rs.Add(NewWorkerResult(3, 10, 1_000_000_000))
	rs.Add(NewWorkerResult(4, 10, 1_000_000_000))
	if rs.MessagesPerSecond != 40 {
		t.Fatalf("Expected %v, got %v", 40, rs.MessagesPerSecond)
	}

	rs = NewResults()
	rs.Add(NewWorkerResult(1, 10, 1_000_000_000))
	rs.Add(NewWorkerResult(2, 20, 2_000_000_000))
	if rs.MessagesPerSecond != 20 {
		t.Fatalf("Expected %v, got %v", 20, rs.MessagesPerSecond)
	}
}
