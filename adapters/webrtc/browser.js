// Install message handlers before negotiating so the Prime open response is visible.
export async function connectPrime(onMessage, tracks = [], options = {}) {
  const pc = new RTCPeerConnection(options.rtcConfig ?? { iceServers: [] });
  const dc = pc.createDataChannel("prime", { ordered: true });
  dc.binaryType = "arraybuffer";
  dc.onmessage = onMessage;
  if (options.onTrack) pc.ontrack = options.onTrack;
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
    const returns = [];
    for (const track of tracks) {
      const tx = pc.addTransceiver(track, { direction: options.returnMedia ? "sendrecv" : "sendonly" });
      if (options.returnMedia) {
        if (returns.includes(track.kind)) throw Error("returnMedia accepts one track per kind");
        returns.push(track.kind);
        const mime = track.kind === "audio" ? "audio/opus" : "video/vp8";
        const codecs = RTCRtpSender.getCapabilities(track.kind).codecs.filter(c => c.mimeType.toLowerCase() === mime);
        if (!codecs.length) throw Error("Unavailable return codec " + mime);
        tx.setCodecPreferences(codecs);
      }
    }
    const endpoint = returns.length ? "/offer?returnMedia=" + returns.join(",") : "/offer";
    await pc.setLocalDescription(await pc.createOffer());
    await waitFor(pc, "icegatheringstatechange", () => pc.iceGatheringState === "complete");
    const response = await fetch(endpoint, {
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
