package main

import (
	"context"
	"errors"
	"fmt"
	"io"
	"math"
	"net/http"
	"sync"
	"time"
	"unicode/utf8"

	"github.com/pion/logging"
	"github.com/pion/webrtc/v4"
)

const (
	maxPeers       = 99
	eventHighWater = 128
)

type hub struct {
	api     *webrtc.API
	config  webrtc.Configuration
	mu      sync.Mutex // serializes registry changes and event order for Prime
	next    uint64
	peers   map[uint64]*peer
	events  chan event
	stopped bool
	closing sync.WaitGroup
}

type peer struct {
	id       uint64
	pc       *webrtc.PeerConnection
	channel  *webrtc.DataChannel
	deadline *time.Timer
	opened   bool
	media    map[string]string
	returns  map[string]*webrtc.TrackLocalStaticRTP
}

func newHub(config webrtc.Configuration, settings webrtc.SettingEngine) *hub {
	if len(config.ICEServers) > 0 {
		// Protect TURN credentials even if verbose SDK logging is configured externally.
		settings.LoggerFactory = &logging.DefaultLoggerFactory{Writer: io.Discard}
	}
	settings.SetICETimeouts(4*time.Second, 8*time.Second, time.Second)
	settings.SetSCTPMaxMessageSize(16384)
	return &hub{
		config: config,
		api:    webrtc.NewAPI(webrtc.WithSettingEngine(settings)),
		peers:  make(map[uint64]*peer), events: make(chan event, eventHighWater+4*maxPeers),
	}
}

// All producers hold mu. Once the high-water mark is reached, no more peers
// or messages are admitted. Four reserved lifecycle events per existing peer
// (open, audio, video, close) can still be delivered in order.
func (h *hub) queue(e event) {
	h.events <- e
}

func (h *hub) admit() (*peer, int, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.stopped || h.next == math.MaxInt64 || len(h.peers) >= maxPeers || len(h.events) >= eventHighWater {
		return nil, http.StatusServiceUnavailable, errors.New("no free peer slots")
	}
	pc, err := h.api.NewPeerConnection(h.config)
	if err != nil {
		return nil, http.StatusInternalServerError, errors.New("cannot create peer connection with host ICE configuration")
	}
	h.next++
	p := &peer{id: h.next, pc: pc, media: make(map[string]string)}
	h.peers[p.id] = p
	p.deadline = time.AfterFunc(15*time.Second, func() {
		h.mu.Lock()
		defer h.mu.Unlock()
		if !p.opened {
			h.remove(p, "open-timeout")
		}
	})
	pc.OnConnectionStateChange(func(state webrtc.PeerConnectionState) {
		if state == webrtc.PeerConnectionStateFailed || state == webrtc.PeerConnectionStateClosed {
			h.close(p, state.String())
		}
	})
	pc.OnDataChannel(func(dc *webrtc.DataChannel) { h.attach(p, dc) })
	pc.OnTrack(func(track *webrtc.TrackRemote, _ *webrtc.RTPReceiver) {
		h.consumeTrack(p, track)
	})
	return p, http.StatusOK, nil
}

// The SDK dispatches the open callback on a goroutine. A first message also
// proves the channel is open and can reach us before that callback runs.
func (h *hub) opened(p *peer) {
	if p.opened {
		return
	}
	p.deadline.Stop()
	p.opened = true
	h.queue(event{"open", p.id, ""})
	for kind, codec := range p.media {
		h.queue(event{kind, p.id, codec})
	}
}

func (h *hub) attach(p *peer, dc *webrtc.DataChannel) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.peers[p.id] != p {
		return
	}
	if p.channel != nil || dc.Label() != "prime" || !dc.Ordered() || dc.MaxRetransmits() != nil || dc.MaxPacketLifeTime() != nil {
		h.remove(p, "unsupported-channel")
		return
	}
	p.channel = dc
	dc.OnOpen(func() {
		h.mu.Lock()
		defer h.mu.Unlock()
		if h.peers[p.id] == p {
			h.opened(p)
		}
	})
	dc.OnMessage(func(msg webrtc.DataChannelMessage) {
		h.mu.Lock()
		defer h.mu.Unlock()
		if h.peers[p.id] != p {
			return
		}
		h.opened(p)
		if len(h.events) >= eventHighWater {
			h.remove(p, "event-backlog")
			return
		}
		if len(msg.Data) > maxPayload || (msg.IsString && !utf8.Valid(msg.Data)) {
			h.remove(p, "invalid-message")
			return
		}
		h.queue(messageEvent(p.id, msg.Data, msg.IsString))
	})
	dc.OnClose(func() { h.close(p, "channel-closed") })
	dc.OnError(func(_ error) { h.close(p, "channel-error") })
}

func (h *hub) close(p *peer, reason string) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.remove(p, reason)
}

// Caller owns h.mu. Pion can call back during Close, so release it off-lock.
func (h *hub) remove(p *peer, reason string) {
	if h.peers[p.id] != p {
		return
	}
	delete(h.peers, p.id)
	p.deadline.Stop()
	if p.opened {
		h.queue(event{"close", p.id, reason})
	}
	h.closing.Add(1)
	go func() {
		defer h.closing.Done()
		_ = p.pc.Close()
	}()
}

func (h *hub) send(c command) {
	h.mu.Lock()
	p := h.peers[c.peer]
	if p == nil || !p.opened {
		h.mu.Unlock()
		return // A reply may race a disconnected browser.
	}
	if c.kind == "close" || p.channel.BufferedAmount() >= 256*1024 {
		reason := "prime-close"
		if c.kind != "close" {
			reason = "slow-receiver"
		}
		h.remove(p, reason)
		h.mu.Unlock()
		return
	}
	dc := p.channel
	h.mu.Unlock()
	var err error
	if c.kind == "bytes" {
		err = dc.Send(c.data)
	} else {
		err = dc.SendText(string(c.data))
	}
	if err != nil {
		h.close(p, "send-error")
	}
}

func (h *hub) shutdown() {
	h.mu.Lock()
	h.stopped = true
	for _, p := range h.peers {
		h.remove(p, "shutdown")
	}
	h.mu.Unlock()
	h.closing.Wait()
}

func (h *hub) answer(ctx context.Context, offer webrtc.SessionDescription, returns []string) (*peer, *webrtc.SessionDescription, int, error) {
	p, status, err := h.admit()
	if err != nil {
		return nil, nil, status, err
	}
	if err = p.addReturnTracks(returns); err != nil {
		h.close(p, "media-setup")
		return nil, nil, http.StatusBadRequest, err
	}
	if err = p.pc.SetRemoteDescription(offer); err != nil {
		h.close(p, "invalid-offer")
		return nil, nil, http.StatusBadRequest, fmt.Errorf("invalid offer: %w", err)
	}
	gathered := webrtc.GatheringCompletePromise(p.pc)
	answer, err := p.pc.CreateAnswer(nil)
	if err == nil {
		err = p.pc.SetLocalDescription(answer)
	}
	if err != nil {
		h.close(p, "answer-error")
		return nil, nil, http.StatusBadRequest, fmt.Errorf("cannot answer offer: %w", err)
	}
	select {
	case <-ctx.Done():
		h.close(p, "signaling-timeout")
		return nil, nil, http.StatusRequestTimeout, ctx.Err()
	case <-gathered:
		return p, p.pc.LocalDescription(), http.StatusOK, nil
	}
}
