"""Play a move over each transport on a fresh, published two-human arena (--tick-ms 0)."""
import argparse
import base64
import http.client
import json
import os
import ssl
from urllib.parse import urlsplit

from selenium.webdriver import Keys
from selenium.webdriver.common.by import By

from test_browser import at, browser, connected, key, ui, wait


def check_http(origin, ca_file):
    url = urlsplit(origin)
    tls = ssl.create_default_context(cafile=ca_file)

    def request(method, path, headers):
        conn = (http.client.HTTPSConnection(url.hostname, url.port, context=tls, timeout=8)
                if url.scheme == "https" else http.client.HTTPConnection(url.hostname, url.port, timeout=8))
        try:
            conn.request(method, path, headers=headers)
            response = conn.getresponse()
            return response.status, response.read()
        finally:
            conn.close()

    assert request("GET", "/healthz", {}) == (200, b"")
    status, body = request("GET", "/config", {})
    assert status == 200 and json.loads(body)["ws"] == "ws" + origin[4:] + "/ws"
    wrong = "http://other.invalid"
    assert request("GET", "/config", {"Host": "other.invalid"})[0] == 403
    for headers in ({"Origin": wrong}, {"Origin": origin, "Host": "other.invalid"}, {}):
        assert request("POST", "/offer", headers)[0] == 403
        upgrade = {"Connection": "Upgrade", "Upgrade": "websocket", "Sec-WebSocket-Version": "13",
                   "Sec-WebSocket-Key": base64.b64encode(os.urandom(16)).decode(), **headers}
        assert request("GET", "/ws", upgrade)[0] == 403


def play(origin, ice_ip, ice_port):
    with browser() as driver:
        driver.get(origin)
        connected(driver, 1)
        assert driver.execute_script("return wire instanceof WebSocket")
        ws = driver.current_window_handle
        driver.switch_to.new_window("tab")
        rtc = driver.current_window_handle
        # Observe the peer connection created by the arena's real browser helper.
        driver.execute_cdp_cmd("Page.addScriptToEvaluateOnNewDocument", {"source": """
            window.RTCPeerConnection = class extends RTCPeerConnection {
                createDataChannel(...args) {
                    window.arenaPeer = this;
                    return window.wire = super.createDataChannel(...args);
                }
            };
        """})
        driver.get(origin + "/?transport=rtc")
        connected(driver, 2)
        assert driver.execute_script("return wire instanceof RTCDataChannel && wire.ordered")
        remote = driver.execute_async_script("""
            const done = arguments[0];
            arenaPeer.getStats().then(stats => {
                const transport = [...stats.values()].find(s => s.selectedCandidatePairId);
                const pair = stats.get(transport.selectedCandidatePairId);
                const remote = stats.get(pair.remoteCandidateId);
                done([remote.protocol, remote.address, remote.port, pair.state]);
            }).catch(err => done(String(err)));
        """)
        assert remote == ["udp", ice_ip, ice_port, "succeeded"], remote
        driver.find_element(By.ID, "start").click()
        wait(driver, lambda d: ui(d, "phase") == "Match in play")
        key(driver, Keys.ARROW_RIGHT)
        first = at(driver, 1)
        driver.switch_to.window(ws)
        assert first == at(driver, 1)
        key(driver, Keys.ARROW_LEFT)
        second = at(driver, 2)
        driver.switch_to.window(rtc)
        assert second == at(driver, 2) and second != first
        print(f"{origin} WS and RTC agreed on tick 2, SHA-256 {second}, selected UDP {ice_ip}:{ice_port}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--origin", required=True)
    parser.add_argument("--ice-ip", required=True)
    parser.add_argument("--ice-port", type=int, default=8001)
    parser.add_argument("--ca-file", help="private CA PEM for the HTTP probe (Chromium uses its trust store)")
    args = parser.parse_args()
    check_http(args.origin, args.ca_file)
    play(args.origin, args.ice_ip, args.ice_port)
