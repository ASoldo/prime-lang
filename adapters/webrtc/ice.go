package main

import (
	"encoding/json"
	"errors"
	"os"

	"github.com/pion/webrtc/v4"
)

// Credentials stay in the adapter process. The signaling endpoint never
// serializes this configuration or includes configuration contents in errors.
func loadICE(path string) (webrtc.Configuration, error) {
	var config webrtc.Configuration
	if path == "" {
		return config, nil
	}
	raw, err := os.ReadFile(path)
	if err != nil {
		return config, err
	}
	var ice struct {
		Servers []webrtc.ICEServer `json:"iceServers"`
		Policy  string             `json:"iceTransportPolicy"`
	}
	if json.Unmarshal(raw, &ice) != nil {
		return config, errors.New("invalid ICE configuration file")
	}
	if len(ice.Servers) == 0 {
		return config, errors.New("ICE configuration must contain iceServers")
	}
	switch ice.Policy {
	case "", "all":
	case "relay":
		config.ICETransportPolicy = webrtc.ICETransportPolicyRelay
	default:
		return config, errors.New("invalid ICE transport policy")
	}
	config.ICEServers = ice.Servers
	return config, nil
}
