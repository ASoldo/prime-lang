package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"syscall"
	"time"
)

type state struct {
	ID    int32 `json:"id"`
	Value int32 `json:"value"`
}

type prime struct {
	cmd    *exec.Cmd
	in     io.WriteCloser
	out    *os.File
	reader *bufio.Reader
	done   chan error
}

func launch(argv []string) (*prime, error) {
	cmd := exec.Command(argv[0], argv[1:]...)
	// A terminal interrupt goes to the adapter, which asks Prime to stop.
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	cmd.Stderr = os.Stderr
	in, err := cmd.StdinPipe()
	if err != nil {
		return nil, err
	}
	out, writer, err := os.Pipe()
	if err != nil {
		in.Close()
		return nil, err
	}
	cmd.Stdout = writer
	err = cmd.Start()
	writer.Close()
	if err != nil {
		in.Close()
		out.Close()
		return nil, err
	}
	p := &prime{cmd, in, out, bufio.NewReader(out), make(chan error, 1)}
	go func() { p.done <- cmd.Wait(); close(p.done) }()
	return p, nil
}

func (p *prime) line(timeout time.Duration) (string, error) {
	if err := p.out.SetReadDeadline(time.Now().Add(timeout)); err != nil {
		return "", err
	}
	line, err := p.reader.ReadSlice('\n')
	return strings.TrimSuffix(string(line), "\n"), err
}

func parseState(line, tag string) (state, error) {
	fields := strings.Fields(line)
	if len(fields) != 3 || fields[0] != tag {
		return state{}, fmt.Errorf("expected %s, received %q", tag, line)
	}
	id, err := strconv.ParseInt(fields[1], 10, 32)
	value, e := strconv.ParseInt(fields[2], 10, 32)
	if err != nil || e != nil || id < 0 {
		return state{}, errors.New("invalid Prime snapshot")
	}
	return state{int32(id), int32(value)}, nil
}

// Only one request at a time owns the two pipes.
func (p *prime) exchange(request string) (string, error) {
	if _, err := io.WriteString(p.in, request+"\n"); err != nil {
		return "", err
	}
	return p.line(3 * time.Second)
}

func (p *prime) reap() {
	p.in.Close()
	p.out.Close()
	select {
	case <-p.done:
	case <-time.After(2 * time.Second):
		p.cmd.Process.Kill()
		<-p.done
	}
}
