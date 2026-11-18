use super::types::TypeExpr;
use crate::target::BuildTarget;

#[derive(Clone, Copy)]
pub(crate) enum TcpBuiltin {
    Listen,
    Accept,
    NextHeader,
    ReceiveUntil,
    StartsWith,
    Write,
    Disconnect,
    Close,
}

impl TcpBuiltin {
    pub const ALL: [Self; 8] = [
        Self::Listen,
        Self::Accept,
        Self::NextHeader,
        Self::ReceiveUntil,
        Self::StartsWith,
        Self::Write,
        Self::Disconnect,
        Self::Close,
    ];

    pub fn available(target: &BuildTarget, no_std: bool) -> bool {
        cfg!(feature = "std-builtins") && !no_std && !target.is_embedded()
    }

    pub fn lookup(name: &str) -> Option<Self> {
        if !name.starts_with("tcp_") {
            return None;
        }
        Self::ALL.into_iter().find(|op| op.name() == name)
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Listen => "tcp_listen",
            Self::Accept => "tcp_accept",
            Self::NextHeader => "tcp_next_header",
            Self::ReceiveUntil => "tcp_receive_until",
            Self::StartsWith => "tcp_starts_with",
            Self::Write => "tcp_write",
            Self::Disconnect => "tcp_disconnect",
            Self::Close => "tcp_close",
        }
    }

    pub fn parameter(self) -> Option<(&'static str, TypeExpr)> {
        match self {
            Self::NextHeader => Some(("limit", TypeExpr::named("int32"))),
            Self::Listen => Some(("port", TypeExpr::named("int32"))),
            Self::ReceiveUntil => Some(("delimiter", TypeExpr::named("string"))),
            Self::StartsWith => Some(("prefix", TypeExpr::named("string"))),
            Self::Write => Some(("text", TypeExpr::named("string"))),
            Self::Accept | Self::Disconnect | Self::Close => None,
        }
    }

    pub fn parameter_label(self) -> Option<String> {
        self.parameter()
            .map(|(name, ty)| format!("{name}: {}", ty.canonical_name()))
    }

    pub fn signature(self) -> String {
        format!(
            "fn {}({}) -> {}",
            self.name(),
            self.parameter_label().unwrap_or_default(),
            self.result().canonical_name()
        )
    }

    pub fn result(self) -> TypeExpr {
        match self {
            Self::Listen => TypeExpr::named("int32"),
            Self::Disconnect | Self::Close => TypeExpr::Unit,
            _ => TypeExpr::named("bool"),
        }
    }
}
