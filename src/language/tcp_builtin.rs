use super::types::TypeExpr;
use crate::target::BuildTarget;

#[derive(Clone, Copy)]
pub(crate) enum TcpBuiltin {
    Listen,
    Accept,
    NextFrame,
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
        Self::NextFrame,
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
            Self::NextFrame => "tcp_next_frame",
            Self::ReceiveUntil => "tcp_receive_until",
            Self::StartsWith => "tcp_starts_with",
            Self::Write => "tcp_write",
            Self::Disconnect => "tcp_disconnect",
            Self::Close => "tcp_close",
        }
    }

    pub fn parameters(self) -> &'static [(&'static str, &'static str)] {
        match self {
            Self::Listen => &[("port", "int32")],
            Self::NextFrame => &[("delimiter", "string"), ("limit", "int32")],
            Self::ReceiveUntil => &[("delimiter", "string")],
            Self::StartsWith => &[("prefix", "string")],
            Self::Write => &[("text", "string")],
            Self::Accept | Self::Disconnect | Self::Close => &[],
        }
    }

    pub fn parameter_labels(self) -> impl Iterator<Item = String> {
        self.parameters()
            .iter()
            .map(|(name, ty)| format!("{name}: {ty}"))
    }

    pub fn signature(self) -> String {
        format!(
            "fn {}({}) -> {}",
            self.name(),
            self.parameter_labels().collect::<Vec<_>>().join(", "),
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
