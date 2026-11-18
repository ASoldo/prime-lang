use super::types::TypeExpr;

#[derive(Clone, Copy)]
pub(crate) enum TcpBuiltin {
    Listen,
    Accept,
    ReceiveUntil,
    StartsWith,
    Write,
    Disconnect,
    Close,
}

impl TcpBuiltin {
    pub fn lookup(name: &str) -> Option<Self> {
        Some(match name {
            "tcp_listen" => Self::Listen,
            "tcp_accept" => Self::Accept,
            "tcp_receive_until" => Self::ReceiveUntil,
            "tcp_starts_with" => Self::StartsWith,
            "tcp_write" => Self::Write,
            "tcp_disconnect" => Self::Disconnect,
            "tcp_close" => Self::Close,
            _ => return None,
        })
    }

    pub fn parameter(self) -> Option<TypeExpr> {
        match self {
            Self::Listen => Some(TypeExpr::named("int32")),
            Self::ReceiveUntil | Self::StartsWith | Self::Write => Some(TypeExpr::named("string")),
            Self::Accept | Self::Disconnect | Self::Close => None,
        }
    }

    pub fn result(self) -> TypeExpr {
        match self {
            Self::Listen => TypeExpr::named("int32"),
            Self::Disconnect | Self::Close => TypeExpr::Unit,
            _ => TypeExpr::named("bool"),
        }
    }
}
