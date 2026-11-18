//! Single-task ESP-IDF component socket ABI. Every operation returns a signed int32.
use super::types::TypeExpr;

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum Argument {
    Int,
    Bytes,
}

impl Argument {
    pub fn ty(self) -> TypeExpr {
        match self {
            Self::Int => TypeExpr::named("int32"),
            Self::Bytes => TypeExpr::Slice(Box::new(TypeExpr::named("uint8"))),
        }
    }
}

pub(crate) fn parameters(name: &str) -> Option<&'static [Argument]> {
    use Argument::{Bytes, Int};
    Some(match name {
        "idf_tcp_listen" | "idf_udp_bind" | "idf_local_port" | "idf_sender_port" | "idf_close" => {
            &[Int]
        }
        "idf_tcp_connect" | "idf_tcp_accept" | "idf_rx_byte" => &[Int, Int],
        "idf_tcp_read" | "idf_udp_recv_from" => &[Int, Int, Int],
        "idf_tcp_write" => &[Int, Bytes, Int],
        "idf_udp_send_to" => &[Int, Int, Bytes, Int],
        _ => return None,
    })
}
