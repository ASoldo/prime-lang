// Exercise a real Chromium EventSource over its same-origin HTTP connection.
// Chromium's DevTools pipe uses zero-terminated JSON on inherited fds 3 and 4.
import {spawn} from 'node:child_process';
import assert from 'node:assert/strict';
import {once} from 'node:events';

const [chrome, profile, origin] = process.argv.slice(2);
const browser = spawn(chrome, ['--headless', '--no-first-run', '--disable-background-networking',
    '--disable-extensions', '--disable-gpu', '--remote-debugging-pipe', '--user-data-dir=' + profile],
    {stdio: ['ignore', 'ignore', 'inherit', 'pipe', 'pipe']});
const pending = new Map();
let seq = 0;
let buffer = '';
browser.stdio[4].setEncoding('utf8');
browser.stdio[4].on('data', chunk => {
    buffer += chunk;
    for (;;) {
        const end = buffer.indexOf('\0');
        if (end < 0) break;
        const reply = JSON.parse(buffer.slice(0, end));
        buffer = buffer.slice(end + 1);
        const callback = pending.get(reply.id);
        if (callback) {
            pending.delete(reply.id);
            callback(reply);
        }
    }
});
function command(method, params = {}, sessionId) {
    const id = ++seq;
    return new Promise((resolve, reject) => {
        pending.set(id, reply => reply.error ? reject(Error(reply.error.message)) : resolve(reply.result));
        browser.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + '\0');
    });
}

const pause = ms => new Promise(resolve => setTimeout(resolve, ms));
async function waitFor(predicate, description) {
    const deadline = Date.now() + 12000;
    while (Date.now() < deadline) {
        if (await predicate()) return;
        await pause(40);
    }
    throw Error('timed out awaiting ' + description);
}

const timeout = setTimeout(() => {browser.kill('SIGKILL'); throw Error('Chromium timed out');}, 30000);
try {
    const {targetId} = await command('Target.createTarget', {url: origin + '/state'});
    const {sessionId} = await command('Target.attachToTarget', {targetId, flatten: true});
    async function evaluate(expression) {
        const reply = await command('Runtime.evaluate', {expression, returnByValue: true}, sessionId);
        if (reply.exceptionDetails) throw Error(JSON.stringify(reply.exceptionDetails));
        return reply.result.value;
    }
    await waitFor(() => evaluate('location.origin === ' + JSON.stringify(origin)), 'same-origin page');
    await evaluate(`window.events = []; window.opens = 0;
        window.source = new EventSource('/events');
        source.onopen = () => opens++;
        for (const name of ['reset','promjena']) source.addEventListener(name, e => {
            events.push([e.type, e.lastEventId, JSON.parse(e.data)]);
        }); true;`);
    await waitFor(() => evaluate('events.length === 1'), 'initial snapshot');
    const [[type, id]] = await evaluate('events');
    assert.equal(type, 'reset');
    const initial = Number(id);
    // The first stream ends after the initial snapshot and 15 mutations.
    // Four more writes land while the browser schedules its automatic retry.
    for (let i = 1; i <= 19; ++i) {
        const response = await fetch(origin + '/state', {method:'POST',
            headers:{'Content-Type':'application/json'}, body:JSON.stringify({value:i})});
        assert.equal(response.status, 200);
        assert.deepEqual(await response.json(), {id:initial+i,value:i});
        assert.equal((await (await fetch(origin + '/state')).json()).id, initial+i);
        if (i === 15) await waitFor(() => evaluate('events.length === 16'), 'first stream rollover');
    }
    await waitFor(() => evaluate('events.length >= 20'), 'browser replay');
    assert.ok(await evaluate('opens >= 2'));
    const changes = await evaluate('source.close(); events.slice(1)');
    assert.deepEqual(changes, Array.from({length:19}, (_,i) => ['promjena',String(initial+i+1),
        {id:initial+i+1,value:i+1,label:'očitanje'}]));
} finally {
    const closed = once(browser, 'close');
    browser.kill();
    await closed;
    clearTimeout(timeout);
}
