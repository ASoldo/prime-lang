package main

import (
	"context"
	_ "embed"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"mime"
	"net"
	"net/http"
	"os"
	"os/signal"
	"strconv"
	"syscall"
	"time"

	"github.com/pion/webrtc/v4"
)

//go:embed browser.js
var browserJS []byte

//go:embed index.html
var indexHTML []byte

func (h *hub) offer(w http.ResponseWriter, r *http.Request) {
	contentType, _, err := mime.ParseMediaType(r.Header.Get("Content-Type"))
	if err != nil || contentType != "application/json" {
		http.Error(w, "expected application/json", http.StatusUnsupportedMediaType)
		return
	}
	body, err := io.ReadAll(http.MaxBytesReader(w, r.Body, 64*1024))
	if err != nil {
		http.Error(w, "cannot read offer (maximum 64 KiB)", http.StatusRequestEntityTooLarge)
		return
	}
	var desc webrtc.SessionDescription
	if json.Unmarshal(body, &desc) != nil || desc.Type != webrtc.SDPTypeOffer || desc.SDP == "" {
		http.Error(w, "expected a JSON SDP offer", http.StatusBadRequest)
		return
	}
	ctx, cancel := context.WithTimeout(r.Context(), 5*time.Second)
	defer cancel()
	returns, err := returnKinds(r.URL.Query().Get("returnMedia"))
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	p, answer, status, err := h.answer(ctx, desc, returns)
	if err != nil {
		http.Error(w, err.Error(), status)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(answer); err != nil {
		h.close(p, "signaling-error")
	}
}

func routes(host string, h *hub) http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /{$}", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		_, _ = w.Write(indexHTML)
	})
	mux.HandleFunc("GET /browser.js", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/javascript; charset=utf-8")
		_, _ = w.Write(browserJS)
	})
	mux.HandleFunc("POST /offer", h.offer)
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		origin := r.Header.Get("Origin")
		if r.Host != host || (origin != "" && origin != "http://"+host) {
			http.Error(w, "expected local origin", http.StatusForbidden)
			return
		}
		mux.ServeHTTP(w, r)
	})
}

func run(port int, icePath string, icePort int, advertised string, args []string) error {
	if len(args) == 0 || port < 0 || port > 65535 {
		return errors.New("usage: prime-webrtc [-port 0..65535] [-ice-config FILE] [-ice-udp-port PORT [-ice-advertise-ip IPv4]] -- PROGRAM [ARG ...]")
	}
	if icePort != 0 && icePath != "" {
		return errors.New("the fixed UDP ICE-lite profile requires direct candidates")
	}
	config, err := loadICE(icePath)
	if err != nil {
		return err
	}
	listener, err := net.Listen("tcp4", net.JoinHostPort("127.0.0.1", strconv.Itoa(port)))
	if err != nil {
		return err
	}
	defer listener.Close()
	parent, release := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer release()
	ctx, stop := context.WithCancelCause(parent)
	defer stop(nil)
	settings, mux, err := transportSettings(icePort, advertised)
	if err != nil {
		return err
	}
	if mux != nil {
		defer mux.Close()
	}
	h := newHub(config, settings)
	host := listener.Addr().String()
	server := &http.Server{
		Handler: routes(host, h), ReadHeaderTimeout: 3 * time.Second,
		ReadTimeout: 5 * time.Second, WriteTimeout: 10 * time.Second, IdleTimeout: 15 * time.Second,
		MaxHeaderBytes: 16 * 1024,
	}
	go func() {
		if err := server.Serve(listener); err != nil && !errors.Is(err, http.ErrServerClosed) {
			stop(err)
		}
	}()
	fmt.Println("LISTEN http://" + host)
	done := make(chan error, 1)
	go func() {
		done <- runProgram(ctx, stop, args, h)
		stop(nil)
	}()
	<-ctx.Done()
	server.Close()
	h.shutdown()
	err = <-done
	if cause := context.Cause(ctx); !errors.Is(cause, context.Canceled) {
		return cause
	}
	return err
}

func main() {
	port := flag.Int("port", 0, "IPv4 loopback HTTP port (0 chooses a free port)")
	ice := flag.String("ice-config", "", "private host ICE configuration JSON file")
	icePort := flag.Int("ice-udp-port", 0, "fixed IPv4 ICE UDP port on bridge interfaces (0 uses loopback candidates)")
	advertised := flag.String("ice-advertise-ip", "", "IPv4 address for ICE host candidates behind a port forward")
	flag.Parse()
	if err := run(*port, *ice, *icePort, *advertised, flag.Args()); err != nil {
		log.Fatal(err)
	}
}
