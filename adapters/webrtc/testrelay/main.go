// Command testrelay runs a disposable UDP TURN server for local integration tests.
package main

import (
	"crypto/rand"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/pion/logging"
	"github.com/pion/turn/v5"
)

func run(path string) error {
	socket, err := net.ListenPacket("udp4", "127.0.0.1:0")
	if err != nil {
		return err
	}
	defer socket.Close()
	secret := rand.Text()
	user, password, err := turn.GenerateLongTermTURNRESTCredentials(secret, "test", 5*time.Minute)
	if err != nil {
		return err
	}
	logger := &logging.DefaultLoggerFactory{Writer: io.Discard}
	server, err := turn.NewServer(turn.ServerConfig{
		Realm: "prime.local", LoggerFactory: logger,
		AuthHandler: turn.LongTermTURNRESTAuthHandler(secret, logger.NewLogger("auth")),
		PacketConnConfigs: []turn.PacketConnConfig{{
			PacketConn: socket,
			RelayAddressGenerator: &turn.RelayAddressGeneratorStatic{
				RelayAddress: net.IPv4(127, 0, 0, 1), Address: "127.0.0.1",
			},
			PermissionHandler: func(_ net.Addr, ip net.IP) bool { return ip.IsLoopback() },
		}},
	})
	if err != nil {
		return err
	}
	defer server.Close()
	cfg, err := os.OpenFile(path, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0600)
	if err != nil {
		return err
	}
	defer os.Remove(path)
	err = json.NewEncoder(cfg).Encode(map[string]any{
		"iceTransportPolicy": "relay", "iceServers": []any{map[string]any{
			"urls":     []string{"turn:" + socket.LocalAddr().String() + "?transport=udp"},
			"username": user, "credential": password,
		}},
	})
	closeErr := cfg.Close()
	if err != nil {
		return err
	}
	if closeErr != nil {
		return closeErr
	}
	signals := make(chan os.Signal, 1)
	signal.Notify(signals, os.Interrupt, syscall.SIGTERM)
	defer signal.Stop(signals)
	fmt.Println("READY")
	<-signals
	return nil
}

func main() {
	if len(os.Args) != 2 {
		log.Fatal("usage: testrelay NEW_PRIVATE_CONFIG_FILE")
	}
	if err := run(os.Args[1]); err != nil {
		log.Fatal(err)
	}
}
