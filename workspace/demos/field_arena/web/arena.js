const $ = id => document.getElementById(id);
const config = await (await fetch("/config")).json();
const requested = new URLSearchParams(location.search).get("transport");
const mode = !config.ws || (requested === "rtc" && config.rtc) ? "rtc" : "ws";
const storageKey = "field-arena:" + config.match;
const saved = JSON.parse(sessionStorage.getItem(storageKey) || "null") ||
  { token: crypto.randomUUID(), pending: [], last: 0 };
const save = () => sessionStorage.setItem(storageKey, JSON.stringify(saved));
save();

const colors = ["#141c18","#91cfbe","#e3d785","#bdade0","#a8d67c",
                "#e59576","#9abcda","#dda8b9","#767e78"];
let channel, shutdown, seat = null, state = null, generation = 0, checkedHash = "";
let reconnectTimer, lastSent = 0;
const boards = new Map();
$("transport").textContent = mode === "ws" ? "WebSocket" : "WebRTC · ordered data channel";

function draw(canvas, cells, compact) {
  const ctx = canvas.getContext("2d");
  const unit = canvas.width / 10;
  ctx.fillStyle = colors[0];
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  for (let i = 0; i < cells.length; i++) {
    const color = Number(cells[i]);
    if (compact && !color) continue;
    ctx.fillStyle = color ? colors[color] : "#202a24";
    const x = (i % 10) * unit, y = Math.floor(i / 10) * unit;
    ctx.fillRect(x + 1, y + 1, unit - 1, unit - 1);
    if (color && !compact) {
      ctx.fillStyle = "#ffffff55";
      ctx.fillRect(x + 2, y + 2, unit - 4, 2);
    }
  }
}

function render() {
  const me = state.seats[seat];
  $("player").textContent = "Seat " + (seat + 1) + (me[4] ? " · #" + me[4] : "");
  $("phase").textContent = state.phase === "finished"
    ? "Winner · seat " + (state.winner + 1)
    : state.phase === "lobby" ? "Waiting to start" : me[3] ? "Match in play" : "Eliminated · #" + me[4];
  $("start").hidden = state.phase !== "lobby";
  $("alive").textContent = state.alive + "/" + state.seats.length;
  $("lines").textContent = me[1];
  $("score").textContent = me[2];
  $("tick").textContent = "Tick " + state.tick;
  $("roster").textContent = (state.seats.length - state.bots) + " human seats · " + state.bots + " bots";
  $("frameHash").textContent = "Frame " + checkedHash.slice(0, 12);
  $("frameHash").title = checkedHash;
  status("Connected", true);
  draw($("well"), me[0], false);
  for (const [id, other] of state.seats.entries()) {
    if (id === seat) continue;
    let view = boards.get(id);
    if (!view) {
      const el = document.createElement("figure"), canvas = document.createElement("canvas");
      canvas.width = 60; canvas.height = 120;
      el.className = "opponent";
      el.append(canvas, document.createElement("figcaption"));
      $("opponents").append(el);
      view = { el, canvas };
      boards.set(id, view);
    }
    view.el.dataset.out = String(!other[3]);
    view.el.lastChild.textContent = other[3] ? "S" + (id + 1) : "#" + other[4];
    view.el.title = "Seat " + (id + 1) + (other[3] ? " · alive" : " · eliminated #" + other[4]);
    if (view.cells !== other[0]) draw(view.canvas, other[0], true);
    view.cells = other[0];
  }
  $("events").replaceChildren(...(state.log.length ? state.log : ["No attacks or eliminations yet."]).map(text => {
    const li = document.createElement("li");
    li.textContent = text;
    return li;
  }));
  document.querySelectorAll("[data-action]").forEach(b => b.disabled = state.phase !== "playing" || !me[3]);
}

function sendPending() {
  const open = channel?.readyState === "open" || channel?.readyState === WebSocket.OPEN;
  if (!open || seat === null) return;
  for (const [seq, action] of saved.pending) {
    if (seq > lastSent) {
      channel.send("move " + seq + " " + action);
      lastSent = seq;
    }
  }
}
function move(action) {
  if (!state || state.phase !== "playing" || !state.seats[seat]?.[3] || saved.pending.length >= 16) return;
  saved.pending.push([Math.max(saved.last, saved.pending.at(-1)?.[0] || 0) + 1, action]);
  save();
  sendPending();
}
function status(label, live = false) {
  $("linkState").textContent = label;
  $("linkState").dataset.live = String(live);
}
async function connect() {
  clearTimeout(reconnectTimer);
  const active = ++generation;
  seat = null;
  lastSent = 0;
  let incoming = Promise.resolve(), parts = [], count, digest;
  $("reconnect").hidden = true;
  status("Connecting…");
  function lost(label) {
    if (active !== generation) return;
    ++generation;
    shutdown?.();
    status(label);
    $("reconnect").hidden = false;
    $("start").hidden = true;
    document.querySelectorAll("[data-action]").forEach(b => b.disabled = true);
    reconnectTimer = setTimeout(connect, 1800);
  }
  async function consume(data) {
    if (active !== generation) return;
    if (data.startsWith("seat ")) {
      seat = Number(data.slice(5));
      status("Connected", true);
      return;
    }
    if (data.startsWith("error ")) {
      if (data === "error Seat already connected") return lost("Waiting for previous connection…");
      status(data.slice(6));
      return;
    }
    const record = /^F (\d+) (\d+) ([a-f0-9]{64}) (.*)$/.exec(data);
    if (!record || seat === null) throw Error("Unexpected frame");
    const part = Number(record[1]);
    if (part === 0) { parts = []; count = Number(record[2]); digest = record[3]; }
    if (part !== parts.length || count !== Number(record[2]) || digest !== record[3]) throw Error("Incomplete frame");
    parts.push(record[4]);
    if (parts.length !== count) return;
    const payload = parts.join("");
    const bytes = await crypto.subtle.digest("SHA-256", new TextEncoder().encode(payload));
    const hash = Array.from(new Uint8Array(bytes), b => b.toString(16).padStart(2, "0")).join("");
    if (hash !== digest) throw Error("Invalid snapshot checksum");
    if (active !== generation) return;
    const next = JSON.parse(payload);
    const ack = next.seats[seat][5];
    saved.last = ack;
    saved.pending = next.phase === "playing" && next.seats[seat][3]
      ? saved.pending.filter(([seq]) => seq > ack) : [];
    save();
    checkedHash = hash;
    state = next;
    render();
    sendPending();
  }
  function message({data}) {
    incoming = incoming.then(() => consume(data)).catch(err => lost(err.message));
  }
  try {
    if (mode === "rtc") {
      const { connectPrime } = await import("/browser.js");
      const rtc = await connectPrime(message);
      channel = rtc.dc;
      shutdown = () => rtc.close();
    } else {
      const ws = new WebSocket(config.ws);
      channel = ws;
      shutdown = () => ws.close();
      ws.onmessage = message;
      await new Promise((resolve, reject) => {
        ws.onopen = resolve;
        ws.onerror = () => reject(Error("WebSocket unavailable"));
      });
    }
    channel.onclose = () => lost("Reconnecting…");
    channel.send("join " + saved.token);
  } catch (err) { lost(err.message); }
}
const bindings = { ArrowLeft: 1, ArrowRight: 2, ArrowUp: 3, Space: 4 };
document.addEventListener("keydown", e => {
  if (Object.hasOwn(bindings, e.code) && !(e.code === "Space" && e.target.tagName === "BUTTON")) {
    e.preventDefault();
    move(bindings[e.code]);
  }
});
document.querySelectorAll("[data-action]").forEach(b => b.onclick = () => move(Number(b.dataset.action)));
$("start").onclick = e => { channel.send("start"); e.target.blur(); };
$("reconnect").onclick = connect;
connect();
