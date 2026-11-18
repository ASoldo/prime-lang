//! Bounded, single-client IPv4 loopback transport for host Prime programs.
use std::io::{Read, Write};
use std::net::{Ipv4Addr, TcpListener, TcpStream};
use std::thread;
use std::time::{Duration, Instant};

const LIMIT: usize = 4096;
const IO_TIMEOUT: Duration = Duration::from_secs(2);
const ACCEPT_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Default)]
pub struct TcpServer {
    listener: Option<TcpListener>,
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
            let start = self.received.len().saturating_sub(delimiter.len() - 1);
            self.received.extend_from_slice(&buf[..n]);
            if let Some(i) = self.received[start..]
                .windows(delimiter.len())
                .position(|s| s == delimiter.as_bytes())
            {
                self.received.truncate(start + i + delimiter.len());
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
