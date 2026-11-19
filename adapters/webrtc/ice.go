package main

import (
	"encoding/json"
	"errors"
	"net"
	"os"

	"github.com/pion/ice/v4"
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

// The default adapter only gathers loopback candidates. The deployment profile
// multiplexes every peer on the same forwarded UDP port on bridge interfaces.
func transportSettings(port int, advertised string) (webrtc.SettingEngine, ice.UDPMux, error) {
	var settings webrtc.SettingEngine
	settings.SetNetworkTypes([]webrtc.NetworkType{webrtc.NetworkTypeUDP4})
	if port < 0 || port > 65535 {
		return settings, nil, errors.New("ICE UDP port must be in 0..65535")
	}
	if advertised != "" {
		ip := net.ParseIP(advertised)
		if port == 0 || ip.To4() == nil || ip.IsUnspecified() || ip.IsMulticast() {
			return settings, nil, errors.New("ICE advertised address requires a unicast IPv4 address and a fixed UDP port")
		}
		err := settings.SetICEAddressRewriteRules(webrtc.ICEAddressRewriteRule{
			External: []string{ip.String()}, AsCandidateType: webrtc.ICECandidateTypeHost,
			Mode: webrtc.ICEAddressRewriteReplace,
		})
		if err != nil {
			return settings, nil, err
		}
	}
	if port == 0 {
		settings.SetIncludeLoopbackCandidate(true)
		settings.SetIPFilter(func(ip net.IP) bool { return ip.IsLoopback() })
		return settings, nil, nil
	}
	mux, err := ice.NewMultiUDPMuxFromPort(port, ice.UDPMuxFromPortWithNetworks(ice.NetworkTypeUDP4))
	if err != nil {
		return settings, nil, err
	}
	if len(mux.GetListenAddresses()) == 0 {
		mux.Close()
		return settings, nil, errors.New("no IPv4 interface for the ICE UDP port")
	}
	// The browser checks the advertised endpoint. A successful pair must use
	// the published UDP mapping, including when the server is behind NAT.
	settings.SetLite(true)
	settings.SetICEUDPMux(mux)
	return settings, mux, nil
}
