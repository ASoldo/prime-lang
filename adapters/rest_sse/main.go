// Command rest_sse serves the Prime state machine over IPv4 loopback.
package main

import (
	"errors"
	"flag"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"
)

type listener struct {
	net.Listener
	slots chan struct{}
}

type conn struct {
	net.Conn
	release func()
	once    sync.Once
}

func (c *conn) Close() error {
	err := c.Conn.Close()
	c.once.Do(c.release)
	return err
}

func (l listener) Accept() (net.Conn, error) {
	for {
		c, err := l.Listener.Accept()
		if err != nil {
			return nil, err
		}
		select {
		case l.slots <- struct{}{}:
			return &conn{Conn: c, release: func() { <-l.slots }}, nil
		default:
			c.Close()
		}
	}
}

func run(port int, args []string) error {
	if len(args) == 0 {
		return errors.New("expected a Prime command after --")
	}
	p, err := launch(args)
	if err != nil {
		return err
	}
	defer p.reap()
	line, err := p.line(10 * time.Second)
	if err != nil {
		return fmt.Errorf("waiting for Prime READY: %w", err)
	}
	initial, err := parseState(line, "READY")
	if err != nil {
		return err
	}
	l, err := net.Listen("tcp4", fmt.Sprintf("127.0.0.1:%d", port))
	if err != nil {
		return err
	}
	defer l.Close()
	s := &service{
		worker: p, current: initial,
		changed: make(chan struct{}), subs: make(chan struct{}, 4), failed: make(chan error, 1),
	}
	server := http.Server{
		Handler: s, MaxHeaderBytes: 8192,
		ReadTimeout: 4 * time.Second, ReadHeaderTimeout: 2 * time.Second,
		WriteTimeout: 4 * time.Second, IdleTimeout: 5 * time.Second,
	}
	stop := make(chan os.Signal, 1)
	signal.Notify(stop, os.Interrupt, syscall.SIGTERM)
	defer signal.Stop(stop)
	served := make(chan error, 1)
	go func() { served <- server.Serve(listener{l, make(chan struct{}, 32)}) }()
	fmt.Printf("LISTEN %d\n", l.Addr().(*net.TCPAddr).Port)
	select {
	case <-stop:
	case err = <-served:
	case <-p.done:
		err = errors.New("Prime exited while serving")
	case err = <-s.failed:
	}
	server.Close()
	if err != nil {
		return err
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	line, err = p.exchange("STOP")
	if err != nil {
		return err
	}
	if line != "STOPPED" {
		return fmt.Errorf("expected STOPPED, received %q", line)
	}
	select {
	case err := <-p.done:
		return err
	case <-time.After(2 * time.Second):
		return errors.New("Prime shutdown timed out")
	}
}

func main() {
	port := flag.Int("port", 0, "IPv4 loopback port (0 selects a free port)")
	flag.Parse()
	if err := run(*port, flag.Args()); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
