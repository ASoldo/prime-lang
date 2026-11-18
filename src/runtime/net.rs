//! Numeric-endpoint host sockets, shared by the interpreter and the native ABI.
use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::net::{SocketAddr, TcpListener, TcpStream, UdpSocket};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

pub const MAX_BYTES: usize = 65535;

macro_rules! operations {
    ($( $op:ident => $name:literal ($($arg:literal : $ty:literal),*) -> $result:literal; )*) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        #[repr(u32)]
        pub enum Op { $($op,)* }
        impl Op {
            pub const ALL: &'static [Self] = &[$(Self::$op,)*];
            #[allow(dead_code)] // Used by the CLI, absent in the standalone ABI crate.
            pub fn name(self) -> &'static str { match self { $(Self::$op => $name,)* } }
            pub fn parameters(self) -> &'static [(&'static str, &'static str)] {
                match self { $(Self::$op => &[$(($arg, $ty)),*],)* }
            }
            #[allow(dead_code)]
            pub fn success(self) -> &'static str { match self { $(Self::$op => $result,)* } }
        }
    };
}
operations! {
    Listen => "net_tcp_listen"("endpoint":"string") -> "int32";
    Connect => "net_tcp_connect"("endpoint":"string", "timeout_ms":"int32") -> "int32";
    Accept => "net_tcp_accept"("listener":"int32", "timeout_ms":"int32") -> "int32";
    Read => "net_tcp_read"("stream":"int32", "output":"[]uint8", "limit":"int32", "timeout_ms":"int32") -> "int32";
    Write => "net_tcp_write"("stream":"int32", "bytes":"[]uint8", "timeout_ms":"int32") -> "int32";
    Bind => "net_udp_bind"("endpoint":"string") -> "int32";
    SendTo => "net_udp_send_to"("socket":"int32", "endpoint":"string", "bytes":"[]uint8", "timeout_ms":"int32") -> "int32";
    RecvFrom => "net_udp_recv_from"("socket":"int32", "output":"[]uint8", "limit":"int32", "timeout_ms":"int32") -> "string";
    Local => "net_local_endpoint"("handle":"int32") -> "string";
    Peer => "net_peer_endpoint"("stream":"int32") -> "string";
    Close => "net_close"("handle":"int32") -> "()";
}

enum Socket {
    Listener(TcpListener),
    Stream(TcpStream),
    Datagram(UdpSocket),
}
#[derive(Default)]
struct Table {
    last: i32,
    sockets: HashMap<i32, Arc<Socket>>,
}
#[derive(Default)]
pub struct Network(Mutex<Table>);

enum Arg {
    Int(i32),
    Text(String),
    Bytes(Vec<u8>),
    Output,
}
pub enum Reply {
    Int(i32),
    Text(String),
    Unit,
    Read(Vec<u8>),
    Datagram(String, Vec<u8>),
}

fn endpoint(text: &str) -> io::Result<SocketAddr> {
    text.parse()
        .map_err(|_| io::Error::other("expected a numeric IP endpoint with port"))
}
fn timeout(ms: i32) -> io::Result<Duration> {
    if ms <= 0 {
        return Err(io::Error::other("timeout_ms must be positive"));
    }
    Ok(Duration::from_millis(ms as u64))
}

// Keep descriptors nonblocking so concurrent operations need no shared timeout setting.
fn until_ready<T>(ms: i32, mut op: impl FnMut() -> io::Result<T>) -> io::Result<T> {
    let deadline = Instant::now() + timeout(ms)?;
    loop {
        match op() {
            Err(e)
                if matches!(
                    e.kind(),
                    io::ErrorKind::WouldBlock | io::ErrorKind::Interrupted
                ) => {}
            result => return result,
        }
        let remaining = deadline.saturating_duration_since(Instant::now());
        if remaining.is_zero() {
            return Err(io::ErrorKind::TimedOut.into());
        }
        std::thread::sleep(remaining.min(Duration::from_millis(1)));
        if Instant::now() >= deadline {
            return Err(io::ErrorKind::TimedOut.into());
        }
    }
}

impl Network {
    fn insert(&self, socket: Socket) -> io::Result<Reply> {
        let mut table = self.0.lock().unwrap();
        let id = table
            .last
            .checked_add(1)
            .ok_or_else(|| io::Error::other("socket IDs exhausted"))?;
        table.last = id;
        table.sockets.insert(id, Arc::new(socket));
        Ok(Reply::Int(id))
    }
    fn get(&self, id: i32) -> io::Result<Arc<Socket>> {
        self.0
            .lock()
            .unwrap()
            .sockets
            .get(&id)
            .cloned()
            .ok_or_else(|| io::Error::other("unknown or closed socket"))
    }
    fn call(&self, op: Op, args: &[Arg]) -> io::Result<Reply> {
        use Arg::{Bytes, Int, Output, Text};
        match (op, args) {
            (Op::Listen, [Text(addr)]) => {
                let s = TcpListener::bind(endpoint(addr)?)?;
                s.set_nonblocking(true)?;
                self.insert(Socket::Listener(s))
            }
            (Op::Bind, [Text(addr)]) => {
                let s = UdpSocket::bind(endpoint(addr)?)?;
                s.set_nonblocking(true)?;
                self.insert(Socket::Datagram(s))
            }
            (Op::Connect, [Text(addr), Int(ms)]) => {
                let s = TcpStream::connect_timeout(&endpoint(addr)?, timeout(*ms)?)?;
                s.set_nonblocking(true)?;
                self.insert(Socket::Stream(s))
            }
            (Op::Accept, [Int(id), Int(ms)]) => {
                let socket = self.get(*id)?;
                let Socket::Listener(s) = &*socket else {
                    return Err(io::Error::other("expected TCP listener"));
                };
                let (stream, _) = until_ready(*ms, || s.accept())?;
                stream.set_nonblocking(true)?;
                self.insert(Socket::Stream(stream))
            }
            (Op::Close, [Int(id)]) => {
                self.0
                    .lock()
                    .unwrap()
                    .sockets
                    .remove(id)
                    .ok_or_else(|| io::Error::other("unknown or closed socket"))?;
                Ok(Reply::Unit)
            }
            (Op::Local, [Int(id)]) => {
                let addr = match &*self.get(*id)? {
                    Socket::Listener(s) => s.local_addr(),
                    Socket::Stream(s) => s.local_addr(),
                    Socket::Datagram(s) => s.local_addr(),
                }?;
                Ok(Reply::Text(addr.to_string()))
            }
            (Op::Peer, [Int(id)]) => {
                let socket = self.get(*id)?;
                let Socket::Stream(s) = &*socket else {
                    return Err(io::Error::other("expected TCP stream"));
                };
                Ok(Reply::Text(s.peer_addr()?.to_string()))
            }
            (Op::Write, [Int(id), Bytes(data), Int(ms)]) => {
                let socket = self.get(*id)?;
                let Socket::Stream(s) = &*socket else {
                    return Err(io::Error::other("expected TCP stream"));
                };
                let mut stream = s;
                Ok(Reply::Int(until_ready(*ms, || stream.write(data))? as i32))
            }
            (Op::SendTo, [Int(id), Text(addr), Bytes(data), Int(ms)]) => {
                let socket = self.get(*id)?;
                let Socket::Datagram(s) = &*socket else {
                    return Err(io::Error::other("expected UDP socket"));
                };
                let dest = endpoint(addr)?;
                Ok(Reply::Int(
                    until_ready(*ms, || s.send_to(data, dest))? as i32
                ))
            }
            (Op::Read | Op::RecvFrom, [Int(id), Output, Int(limit), Int(ms)]) => {
                if !(1..=MAX_BYTES as i32).contains(limit) {
                    return Err(io::Error::other("limit outside 1..65535"));
                }
                let socket = self.get(*id)?;
                match (op, &*socket) {
                    (Op::Read, Socket::Stream(s)) => {
                        let mut buf = vec![0; *limit as usize];
                        let mut stream = s;
                        let n = until_ready(*ms, || stream.read(&mut buf))?;
                        buf.truncate(n);
                        Ok(Reply::Read(buf))
                    }
                    (Op::RecvFrom, Socket::Datagram(s)) => {
                        // One extra byte detects truncation even when the OS
                        // silently discards the remainder of an oversized datagram.
                        let mut buf = vec![0; *limit as usize + 1];
                        let (n, source) = until_ready(*ms, || s.recv_from(&mut buf))?;
                        if n > *limit as usize {
                            return Err(io::Error::other("datagram exceeded limit"));
                        }
                        buf.truncate(n);
                        Ok(Reply::Datagram(source.to_string(), buf))
                    }
                    _ => Err(io::Error::other("wrong socket type for receive")),
                }
            }
            _ => Err(io::Error::other("invalid socket arguments")),
        }
    }
}

/// Prime value representations differ between the two backends. Keep argument
/// validation and output-buffer commit semantics identical at their boundary.
pub trait Values {
    type Value;
    fn integer(v: &Self::Value) -> Result<i32, String>;
    fn text(v: &Self::Value) -> Result<String, String>;
    fn bytes(v: &Self::Value) -> Result<Vec<u8>, String>;
    fn replace(v: &Self::Value, bytes: Vec<u8>) -> Result<(), String>;
}
pub fn invoke<V: Values>(network: &Network, op: Op, values: &[V::Value]) -> Result<Reply, String> {
    if values.len() != op.parameters().len() {
        return Err("incorrect socket arity".into());
    }
    let mut args = Vec::with_capacity(values.len());
    for ((_, ty), val) in op.parameters().iter().zip(values) {
        args.push(match *ty {
            "int32" => Arg::Int(V::integer(val)?),
            "string" => Arg::Text(V::text(val)?),
            "[]uint8" if matches!(op, Op::Read | Op::RecvFrom) => Arg::Output,
            "[]uint8" => Arg::Bytes(V::bytes(val)?),
            _ => unreachable!(),
        });
    }
    match network.call(op, &args).map_err(|e| e.to_string())? {
        Reply::Read(data) => {
            let len = data.len() as i32;
            V::replace(&values[1], data)?;
            Ok(Reply::Int(len))
        }
        Reply::Datagram(addr, data) => {
            V::replace(&values[1], data)?;
            Ok(Reply::Text(addr))
        }
        other => Ok(other),
    }
}
