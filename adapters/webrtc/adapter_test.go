package main

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/pion/webrtc/v4"
)

func TestSignalingFailures(t *testing.T) {
	h := newHub(webrtc.Configuration{})
	defer h.shutdown()
	const host = "127.0.0.1:8080"
	handler := routes(host, h)
	for _, tc := range []struct {
		name, host, origin, contentType, body string
		want                                  int
	}{
		{"host", "localhost", "", "application/json", "{}", http.StatusForbidden},
		{"origin", host, "http://example.invalid", "application/json", "{}", http.StatusForbidden},
		{"content type", host, "", "text/plain", "{}", http.StatusUnsupportedMediaType},
		{"size", host, "", "application/json", strings.Repeat("x", 65537), http.StatusRequestEntityTooLarge},
		{"type", host, "", "application/json", `{"type":"answer","sdp":"v=0"}`, http.StatusBadRequest},
		{"sdp", host, "", "application/json", `{"type":"offer","sdp":"invalid"}`, http.StatusBadRequest},
	} {
		t.Run(tc.name, func(t *testing.T) {
			r := httptest.NewRequest("POST", "/offer", strings.NewReader(tc.body))
			r.Host = tc.host
			r.Header.Set("Origin", tc.origin)
			r.Header.Set("Content-Type", tc.contentType)
			w := httptest.NewRecorder()
			handler.ServeHTTP(w, r)
			if w.Code != tc.want {
				t.Fatalf("status %d, want %d", w.Code, tc.want)
			}
		})
	}
	if len(h.peers) != 0 {
		t.Fatal("failed SDP retained a peer slot")
	}
}

func TestPendingPeerDeadlineReleasesCapacity(t *testing.T) {
	h := newHub(webrtc.Configuration{})
	defer h.shutdown()
	var first *peer
	for i := range maxPeers {
		p, _, err := h.admit()
		if err != nil {
			t.Fatal(err)
		}
		if i == 0 {
			first = p
		}
	}
	if _, status, _ := h.admit(); status != http.StatusServiceUnavailable {
		t.Fatalf("admission at capacity returned %d", status)
	}
	first.deadline.Reset(time.Millisecond)
	limit := time.Now().Add(2 * time.Second)
	for {
		p, _, err := h.admit()
		if err == nil {
			if p.id <= first.id {
				t.Fatal("connection ID reused")
			}
			break
		}
		if time.Now().After(limit) {
			t.Fatal("expired unconnected peer still occupies a slot")
		}
		time.Sleep(5 * time.Millisecond)
	}
}

func TestPrimeCommands(t *testing.T) {
	for _, line := range []string{"log", "close 0", "close 1 extra", "text 1",
		"bytes 7 !", "text64 7 /w==", "text 3 " + strings.Repeat("x", maxPayload+1)} {
		if _, err := parseCommand(line); err == nil {
			t.Fatalf("accepted %q", line)
		}
	}
	for _, line := range []string{"text 2 ", "bytes 3 ", "text64 1 AA==", "close 1"} {
		if _, err := parseCommand(line); err != nil {
			t.Fatalf("%q: %v", line, err)
		}
	}
}

func TestShortLivedProgram(t *testing.T) {
	// Reuse the test executable so the child needs no shell.
	if os.Getenv("PRIME_RTC_TEST_CHILD") == "1" {
		_, _ = os.Stdout.WriteString("invalid command\n")
		os.Exit(0)
	}
	t.Setenv("PRIME_RTC_TEST_CHILD", "1")
	ctx, stop := context.WithCancelCause(t.Context())
	defer stop(nil)
	h := newHub(webrtc.Configuration{})
	defer h.shutdown()
	_ = runProgram(ctx, stop, []string{os.Args[0], "-test.run=^TestShortLivedProgram$"}, h)
	if cause := context.Cause(ctx); cause == nil || errors.Is(cause, context.Canceled) {
		t.Fatalf("missing final command error: %v", cause)
	}
}

func TestReturnKinds(t *testing.T) {
	for _, input := range []string{"video,video", "unknown", "audio,"} {
		if _, err := returnKinds(input); err == nil {
			t.Fatalf("accepted returnMedia=%s", input)
		}
	}
	kinds, err := returnKinds("audio,video")
	if err != nil || len(kinds) != 2 {
		t.Fatal("combined return rejected")
	}
}
