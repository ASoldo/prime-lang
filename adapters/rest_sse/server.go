package main

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"mime"
	"net/http"
	"strconv"
	"strings"
	"sync"
	"time"
)

const historySize = 16

type service struct {
	mu      sync.Mutex
	worker  *prime
	current state
	history []state
	changed chan struct{}
	subs    chan struct{}
	failed  chan error
}

func problem(w http.ResponseWriter, status int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(map[string]string{"error": http.StatusText(status)})
}

// The wire schema has exactly one required integer member. Token iteration
// preserves duplicate names that decoding into a map or struct would discard.
func valueOf(w http.ResponseWriter, r *http.Request) (int32, int) {
	media, params, err := mime.ParseMediaType(r.Header.Get("Content-Type"))
	if err != nil || media != "application/json" || (params["charset"] != "" && !strings.EqualFold(params["charset"], "utf-8")) || r.Header.Get("Content-Encoding") != "" {
		return 0, 415
	}
	body, err := io.ReadAll(http.MaxBytesReader(w, r.Body, 1024))
	var tooBig *http.MaxBytesError
	if errors.As(err, &tooBig) {
		return 0, 413
	}
	if err != nil {
		return 0, 400
	}
	d := json.NewDecoder(bytes.NewReader(body))
	d.UseNumber()
	begin, e := d.Token()
	if e != nil || begin != json.Delim('{') {
		return 0, 400
	}
	key, e := d.Token()
	if e != nil || key != "value" {
		return 0, 400
	}
	raw, e := d.Token()
	number, ok := raw.(json.Number)
	if e != nil || !ok {
		return 0, 400
	}
	value, e := strconv.ParseInt(string(number), 10, 32)
	if e != nil {
		return 0, 400
	}
	end, e := d.Token()
	if e != nil || end != json.Delim('}') {
		return 0, 400
	}
	if _, e = d.Token(); e != io.EOF {
		return 0, 400
	}
	return int32(value), 0
}

func (s *service) request(command string, update bool) (state, int) {
	s.mu.Lock()
	defer s.mu.Unlock()
	line, err := s.worker.exchange(command)
	if err == nil && line == "REJECT" {
		return state{}, 422
	}
	snapshot := state{}
	if err == nil {
		snapshot, err = parseState(line, "STATE")
	}
	if err != nil {
		select {
		case s.failed <- err:
		default:
		}
		return state{}, 503
	}
	if update {
		s.current = snapshot
		if len(s.history) == historySize {
			s.history = s.history[1:]
		}
		s.history = append(s.history, snapshot)
		close(s.changed)
		s.changed = make(chan struct{})
	}
	return snapshot, 200
}

func (s *service) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if r.Proto != "HTTP/1.1" {
		problem(w, 505)
		return
	}
	if r.URL.Path != "/state" && r.URL.Path != "/events" {
		problem(w, 404)
		return
	}
	command := "GET"
	update := false
	switch {
	case r.Method == "GET":
		if r.ContentLength != 0 || len(r.TransferEncoding) != 0 {
			problem(w, 400)
			return
		}
		if r.URL.Path == "/events" {
			s.events(w, r)
			return
		}
	case r.Method == "POST" && r.URL.Path == "/state":
		value, status := valueOf(w, r)
		if status != 0 {
			problem(w, status)
			return
		}
		command = fmt.Sprintf("SET\n%d", value)
		update = true
	default:
		allow := "GET"
		if r.URL.Path == "/state" {
			allow = "GET, POST"
		}
		w.Header().Set("Allow", allow)
		problem(w, 405)
		return
	}
	val, status := s.request(command, update)
	if status != 200 {
		problem(w, status)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Cache-Control", "no-store")
	json.NewEncoder(w).Encode(val)
}

// Copy a bounded batch and capture the next notification under one lock.
// Subscriber writes and flushes never hold the lock shared with Prime.
func (s *service) batch(cursor int64) ([]state, string, <-chan struct{}) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if cursor < 0 || cursor > int64(s.current.ID) || (len(s.history) > 0 && cursor < int64(s.history[0].ID)-1) {
		return []state{s.current}, "reset", s.changed
	}
	var events []state
	for _, event := range s.history {
		if int64(event.ID) > cursor {
			events = append(events, event)
		}
	}
	return events, "promjena", s.changed
}

func (s *service) events(w http.ResponseWriter, r *http.Request) {
	cursor := int64(-1)
	if ids := r.Header.Values("Last-Event-ID"); len(ids) > 0 {
		var err error
		cursor, err = strconv.ParseInt(ids[0], 10, 32)
		if err != nil || len(ids) != 1 || strconv.FormatInt(cursor, 10) != ids[0] || cursor < 0 {
			problem(w, 400)
			return
		}
	}
	select {
	case s.subs <- struct{}{}:
		defer func() { <-s.subs }()
	default:
		problem(w, 503)
		return
	}
	w.Header().Set("Content-Type", "text/event-stream; charset=utf-8")
	w.Header().Set("Cache-Control", "no-cache")
	controller := http.NewResponseController(w)
	tick := time.NewTicker(10 * time.Second)
	defer tick.Stop()
	sent := 0
	for {
		batch, name, changed := s.batch(cursor)
		controller.SetWriteDeadline(time.Now().Add(2 * time.Second))
		for _, event := range batch {
			// Each field is encoded as UTF-8; JSON encoding keeps data on one line.
			data, _ := json.Marshal(struct {
				state
				Label string `json:"label"`
			}{event, "očitanje"})
			if _, err := fmt.Fprintf(w, "id: %d\nevent: %s\ndata: %s\n\n", event.ID, name, data); err != nil {
				return
			}
			cursor = int64(event.ID)
			sent++
			if sent == historySize {
				break
			}
		}
		if _, err := io.WriteString(w, ": ok\n\n"); err != nil {
			return
		}
		if err := controller.Flush(); err != nil {
			return
		}
		if sent == historySize {
			return
		}
		controller.SetWriteDeadline(time.Time{})
		select {
		case <-r.Context().Done():
			return
		case <-changed:
		case <-tick.C:
		}
	}
}
