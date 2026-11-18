"""Check the SDK component's bounded BSD socket ABI against an independent host UDP peer."""
import ctypes
from pathlib import Path
import socket
import subprocess
import tempfile
import unittest


ADAPTER = Path(__file__).resolve().parents[2] / "workspace/demos/esp32_idf_net/main/socket_adapter.c"


class ComponentSockets(unittest.TestCase):
    def test_receive_boundary_and_handle_lifetime(self):
        with tempfile.TemporaryDirectory() as tmp, socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as peer:
            lib = Path(tmp) / "adapter.so"
            subprocess.run(["cc", "-shared", "-fPIC", "-Wall", "-Werror", ADAPTER, "-o", lib], check=True)
            api = ctypes.CDLL(str(lib))
            peer.bind(("127.0.0.1", 0))
            peer.settimeout(2)
            handle = api.prime_idf_udp_bind(0)
            self.assertGreater(handle, 0)
            dest = ("127.0.0.1", api.prime_idf_local_port(handle))
            try:
                peer.sendto(b"abc", dest)
                self.assertLess(api.prime_idf_udp_recv_from(handle, 2, 1000), 0)
                self.assertLess(api.prime_idf_rx_byte(handle, 0), 0)
                self.assertLess(api.prime_idf_sender_port(handle), 0)

                payload = bytes(range(256))
                peer.sendto(payload, dest)
                self.assertEqual(api.prime_idf_udp_recv_from(handle, 256, 1000), 256)
                self.assertEqual(bytes(api.prime_idf_rx_byte(handle, i) for i in range(256)), payload)
                self.assertLess(api.prime_idf_rx_byte(handle, 256), 0)
                source = api.prime_idf_sender_port(handle)
                self.assertEqual(source, peer.getsockname()[1])
                self.assertEqual(api.prime_idf_udp_send_to(handle, source, payload, 256, 1000), 256)
                self.assertEqual(peer.recvfrom(256), (payload, dest))
                peer.sendto(b"", dest)
                self.assertEqual(api.prime_idf_udp_recv_from(handle, 1, 1000), 0)
                self.assertLess(api.prime_idf_udp_recv_from(handle, 1, 20), 0)
            finally:
                self.assertEqual(api.prime_idf_close(handle), 0)
            replacement = api.prime_idf_udp_bind(dest[1])
            self.assertGreater(replacement, 0)
            self.assertLess(api.prime_idf_close(handle), 0)
            self.assertEqual(api.prime_idf_close(replacement), 0)
