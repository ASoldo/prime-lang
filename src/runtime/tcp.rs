//! Bounded IPv4 loopback transport for host Prime programs.
use std::io::{Read, Write};
use std::net::{Ipv4Addr, TcpListener, TcpStream};
use std::thread;
use std::time::{Duration, Instant};

const FRAME_CAPACITY: usize = 4;
const LIMIT: usize = 4096;
const IO_TIMEOUT: Duration = Duration::from_secs(2);
const ACCEPT_TIMEOUT: Duration = Duration::from_secs(10);

// Return true once the delimiter is complete, including across read boundaries.
fn append_until(frame: &mut Vec<u8>, bytes: &[u8], delimiter: &[u8]) -> bool {
    let start = frame.len().saturating_sub(delimiter.len() - 1);
    frame.extend_from_slice(bytes);
    if let Some(i) = frame[start..]
        .windows(delimiter.len())
        .position(|s| s == delimiter)
    {
        frame.truncate(start + i + delimiter.len());
        return true;
    }
    false
}

struct PendingFrame {
    socket: TcpStream,
    frame: Vec<u8>,
    deadline: Instant,
}

enum FrameState {
    Pending,
    Complete,
    Closed,
}

impl PendingFrame {
    fn poll(&mut self, delimiter: &[u8]) -> FrameState {
        let mut buf = [0; 512];
        while self.frame.len() < LIMIT && Instant::now() < self.deadline {
            let len = buf.len().min(LIMIT - self.frame.len());
            match self.socket.read(&mut buf[..len]) {
                Ok(0) => break,
                Ok(n) => {
                    if append_until(&mut self.frame, &buf[..n], delimiter) {
                        return FrameState::Complete;
                    }
                }
                Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
                    return FrameState::Pending;
                }
                Err(err) if err.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(_) => break,
            }
        }
        FrameState::Closed
    }
}

#[derive(Default)]
pub struct TcpServer {
    listener: Option<TcpListener>,
    frame_delimiter: Option<String>,
    pending: Vec<PendingFrame>,
    frame_admissions: i32,
    client: Option<TcpStream>,
    received: Vec<u8>,
}

impl TcpServer {
    pub fn listen(&mut self, port: i32) -> i32 {
        if self.listener.is_some() {
            return -1;
        }
        let Some((listener, port)) = u16::try_from(port).ok().and_then(|port| {
            let listener = TcpListener::bind((Ipv4Addr::LOCALHOST, port)).ok()?;
            listener.set_nonblocking(true).ok()?;
            let port = listener.local_addr().ok()?.port();
            Some((listener, port))
        }) else {
            return -1;
        };
        self.listener = Some(listener);
        i32::from(port)
    }

    pub fn accept(&mut self) -> bool {
        self.disconnect();
        let Some(listener) = &self.listener else {
            return false;
        };
        let deadline = Instant::now() + ACCEPT_TIMEOUT;
        while Instant::now() < deadline {
            match listener.accept() {
                Ok((client, _)) => {
                    if client.set_nonblocking(false).is_err() {
                        return false;
                    }
                    self.client = Some(client);
                    return true;
                }
                Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => {
                    thread::sleep(Duration::from_millis(5))
                }
                Err(err) if err.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(_) => break,
            }
        }
        false
    }

    // Select the next complete frame as the current client for starts_with/write.
    pub fn next_frame(&mut self, delimiter: &str, limit: i32) -> bool {
        let Some(listener) = &self.listener else {
            return false;
        };
        if delimiter.is_empty() || delimiter.len() > LIMIT || !(1..=256).contains(&limit) {
            return false;
        }

        match &self.frame_delimiter {
            Some(chosen) if chosen != delimiter => return false,
            None => self.frame_delimiter = Some(delimiter.to_owned()),
            _ => {}
        }
        self.client = None;
        self.received.clear();
        let mut idle_deadline = Instant::now() + ACCEPT_TIMEOUT;
        loop {
            while self.pending.len() < FRAME_CAPACITY && self.frame_admissions < limit {
                match listener.accept() {
                    Ok((socket, _)) => {
                        self.frame_admissions += 1;
                        let now = Instant::now();
                        idle_deadline = now + ACCEPT_TIMEOUT;
                        if socket.set_nonblocking(true).is_ok() {
                            self.pending.push(PendingFrame {
                                socket,
                                frame: Vec::new(),
                                deadline: now + IO_TIMEOUT,
                            });
                        }
                    }
                    Err(err) if err.kind() == std::io::ErrorKind::WouldBlock => break,
                    Err(err) if err.kind() == std::io::ErrorKind::Interrupted => break,
                    Err(_) => return false,
                }
            }

            let mut i = 0;
            while i < self.pending.len() {
                match self.pending[i].poll(delimiter.as_bytes()) {
                    FrameState::Pending => i += 1,
                    FrameState::Closed => {
                        self.pending.remove(i);
                    }
                    FrameState::Complete => {
                        let ready = self.pending.remove(i);
                        if ready.socket.set_nonblocking(false).is_ok() {
                            self.client = Some(ready.socket);
                            self.received = ready.frame;
                            return true;
                        }
                    }
                }
            }

            if self.pending.is_empty()
                && (self.frame_admissions >= limit || Instant::now() >= idle_deadline)
            {
                return false;
            }
            thread::sleep(Duration::from_millis(5));
        }
    }

    // Capture a single delimited frame. Discard trailing bytes from the last read.
    pub fn receive_until(&mut self, delimiter: &str) -> bool {
        self.received.clear();
        let Some(client) = &mut self.client else {
            return false;
        };
        if delimiter.is_empty() || delimiter.len() > LIMIT {
            return false;
        }
        let deadline = Instant::now() + IO_TIMEOUT;
        let mut buf = [0; 512];
        while self.received.len() < LIMIT {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() || client.set_read_timeout(Some(remaining)).is_err() {
                break;
            }
            let len = buf.len().min(LIMIT - self.received.len());
            let Ok(n) = client.read(&mut buf[..len]) else {
                break;
            };
            if n == 0 {
                break;
            }
            if append_until(&mut self.received, &buf[..n], delimiter.as_bytes()) {
                return true;
            }
        }
        self.received.clear();
        false
    }

    pub fn starts_with(&self, prefix: &str) -> bool {
        !self.received.is_empty() && self.received.starts_with(prefix.as_bytes())
    }

    pub fn write(&mut self, text: &str) -> bool {
        let Some(client) = &mut self.client else {
            return false;
        };
        if text.len() > LIMIT {
            return false;
        }
        let deadline = Instant::now() + IO_TIMEOUT;
        let mut bytes = text.as_bytes();
        while !bytes.is_empty() {
            let remaining = deadline.saturating_duration_since(Instant::now());
            if remaining.is_zero() || client.set_write_timeout(Some(remaining)).is_err() {
                return false;
            }
            match client.write(bytes) {
                Ok(0) | Err(_) => return false,
                Ok(n) => bytes = &bytes[n..],
            }
        }
        true
    }

    pub fn disconnect(&mut self) {
        self.client = None;
        self.received.clear();
    }

    pub fn close(&mut self) {
        *self = Self::default();
    }
}
