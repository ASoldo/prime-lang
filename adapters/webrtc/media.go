package main

import (
	"errors"
	"strings"

	"github.com/pion/webrtc/v4"
)

func returnKinds(query string) ([]string, error) {
	if query == "" {
		return nil, nil
	}
	kinds := strings.Split(query, ",")
	seen := make(map[string]bool)
	for _, kind := range kinds {
		if (kind != "audio" && kind != "video") || seen[kind] {
			return nil, errors.New("returnMedia accepts one audio and/or one video track")
		}
		seen[kind] = true
	}
	return kinds, nil
}

// Attach local senders before negotiation. Encoded payloads are returned only
// to their source peer. The default receive-only path creates no senders.
func (p *peer) addReturnTracks(kinds []string) error {
	p.returns = make(map[string]*webrtc.TrackLocalStaticRTP, len(kinds))
	for _, kind := range kinds {
		codec := webrtc.RTPCodecCapability{MimeType: webrtc.MimeTypeVP8, ClockRate: 90000}
		if kind == "audio" {
			codec = webrtc.RTPCodecCapability{MimeType: webrtc.MimeTypeOpus, ClockRate: 48000, Channels: 2}
		}
		local, err := webrtc.NewTrackLocalStaticRTP(codec, "return-"+kind, "prime-return")
		if err != nil {
			return err
		}
		sender, err := p.pc.AddTrack(local)
		if err != nil {
			return err
		}
		p.returns[kind] = local
		go func() {
			for {
				if _, _, err := sender.ReadRTCP(); err != nil {
					return
				}
			}
		}()
	}
	return nil
}

func (h *hub) consumeTrack(p *peer, track *webrtc.TrackRemote) {
	kind, codec := track.Kind().String(), track.Codec().MimeType
	var output *webrtc.TrackLocalStaticRTP
	first := true
	for {
		packet, _, err := track.ReadRTP()
		if err != nil {
			return
		}
		if len(packet.Payload) == 0 {
			continue
		}
		if first {
			first = false
			h.mu.Lock()
			if h.peers[p.id] == p {
				if _, seen := p.media[kind]; !seen {
					p.media[kind] = codec
					if p.opened {
						h.queue(event{kind, p.id, codec})
					}
				}
				if local := p.returns[kind]; local != nil && strings.EqualFold(local.Codec().MimeType, codec) {
					output = local
					delete(p.returns, kind)
				}
			}
			h.mu.Unlock()
		}
		if output != nil {
			// The opposite SRTP stream has its own negotiated RTP extensions.
			packet.Header.Extensions = nil
			packet.Header.Extension = false
			if err := output.WriteRTP(packet); err != nil {
				return
			}
		}
	}
}
