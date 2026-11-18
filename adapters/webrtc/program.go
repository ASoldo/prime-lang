package main

import (
	"bufio"
	"context"
	"encoding/base64"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

const maxPayload = 8 * 1024

type event struct {
	kind string
	peer uint64
	data string
}

// The native and interpreted string readers differ on surrounding whitespace.
// Use a printable representation whenever a raw line would lose information.
func messageEvent(id uint64, data []byte, isText bool) event {
	if isText {
		s := string(data)
		if strings.TrimSpace(s) == s && strings.IndexFunc(s, unicode.IsControl) == -1 {
			return event{"text", id, s}
		}
		return event{"text64", id, base64.StdEncoding.EncodeToString(data)}
	}
	return event{"bytes", id, base64.StdEncoding.EncodeToString(data)}
}

type command struct {
	kind string
	peer uint64
	data []byte
}

func parseCommand(line string) (command, error) {
	kind, rest, _ := strings.Cut(line, " ")
	peer, payload, hasPayload := strings.Cut(rest, " ")
	id, err := strconv.ParseUint(peer, 10, 64)
	if err != nil || id == 0 {
		return command{}, errors.New("expected positive peer ID")
	}
	if kind == "close" && !hasPayload {
		return command{kind: kind, peer: id}, nil
	}
	if !hasPayload {
		return command{}, errors.New("missing payload field")
	}
	data := []byte(payload)
	switch kind {
	case "text":
	case "text64", "bytes":
		data, err = base64.StdEncoding.Strict().DecodeString(payload)
	default:
		return command{}, errors.New("unknown Prime command")
	}
	if err != nil || len(data) > maxPayload || (kind != "bytes" && !utf8.Valid(data)) {
		return command{}, errors.New("invalid encoding or oversized Prime payload")
	}
	return command{kind, id, data}, nil
}

// Own both ends explicitly so the application receives EOF on shutdown and a
// stalled reader cannot keep an adapter writer blocked indefinitely.
func runProgram(ctx context.Context, stop context.CancelCauseFunc, args []string, h *hub) error {
	input, writer, err := os.Pipe()
	if err != nil {
		return err
	}
	defer input.Close()
	defer writer.Close()
	output, childOut, err := os.Pipe()
	if err != nil {
		return err
	}
	defer output.Close()
	defer childOut.Close()
	child := exec.Command(args[0], args[1:]...)
	configureChild(child)
	child.Stdin, child.Stdout, child.Stderr = input, childOut, os.Stderr
	if err := child.Start(); err != nil {
		return err
	}
	input.Close()
	childOut.Close()
	readDone := make(chan struct{})
	go func() {
		defer close(readDone)
		scanner := bufio.NewScanner(output)
		scanner.Buffer(make([]byte, 4096), base64.StdEncoding.EncodedLen(maxPayload)+64)
		for scanner.Scan() {
			c, err := parseCommand(scanner.Text())
			if err != nil {
				stop(fmt.Errorf("Prime stdout: %w", err))
				return
			}
			h.send(c)
		}
		if err := scanner.Err(); err != nil {
			stop(fmt.Errorf("Prime stdout: %w", err))
		}
	}()
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case e := <-h.events:
				err := writer.SetWriteDeadline(time.Now().Add(3 * time.Second))
				if err == nil {
					_, err = fmt.Fprintf(writer, "%s\n%d\n%s\n", e.kind, e.peer, e.data)
				}
				if err != nil {
					stop(fmt.Errorf("Prime stdin: %w", err))
					return
				}
			}
		}
	}()
	done := make(chan error, 1)
	go func() { done <- child.Wait() }()
	select {
	case err := <-done:
		// Consume the last complete stdout records before releasing the pipe.
		// A descendant retaining stdout cannot hold a finished program open.
		_ = output.SetReadDeadline(time.Now().Add(time.Second))
		<-readDone
		return err
	case <-ctx.Done():
		writer.Close()
		// Give the Prime input loop time to handle EOF and return.
		select {
		case err := <-done:
			return err
		case <-time.After(2 * time.Second):
			_ = child.Process.Kill()
			<-done
			return errors.New("Prime program exceeded its shutdown deadline")
		}
	}
}
