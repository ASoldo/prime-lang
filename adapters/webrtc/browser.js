// Install message handlers before negotiating so the Prime open response is visible.
export async function connectPrime(onMessage, tracks = []) {
  const pc = new RTCPeerConnection({ iceServers: [] });
  const dc = pc.createDataChannel("prime", { ordered: true });
  dc.binaryType = "arraybuffer";
  dc.onmessage = onMessage;
  const controller = new AbortController();
  const timer = setTimeout(() => {
    controller.abort(new Error("WebRTC setup timed out"));
    pc.close();
  }, 12000);
  const signal = controller.signal;
  function waitFor(target, event, ready) {
    if (ready()) return Promise.resolve();
    return new Promise((resolve, reject) => {
      const update = () => {
        if (!ready()) return;
        target.removeEventListener(event, update);
        signal.removeEventListener("abort", abort);
        resolve();
      };
      const abort = () => {
        target.removeEventListener(event, update);
        reject(signal.reason);
      };
      signal.addEventListener("abort", abort, { once: true });
      target.addEventListener(event, update);
      if (signal.aborted) abort();
    });
  }
  try {
    for (const track of tracks) pc.addTransceiver(track, { direction: "sendonly" });
    await pc.setLocalDescription(await pc.createOffer());
    await waitFor(pc, "icegatheringstatechange", () => pc.iceGatheringState === "complete");
    const response = await fetch("/offer", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(pc.localDescription),
      signal,
    });
    if (!response.ok) throw new Error(await response.text());
    await pc.setRemoteDescription(await response.json());
    await waitFor(dc, "open", () => dc.readyState === "open");
    return { pc, dc, close() { pc.close(); } };
  } catch (error) {
    pc.close();
    throw error;
  } finally {
    clearTimeout(timer);
  }
}
