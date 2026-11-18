use super::*;
use crate::runtime::abi::net::{self, Op, Reply, Values};
struct Interpreted;
impl Values for Interpreted {
    type Value = Value;
    fn integer(v: &Value) -> Result<i32, String> {
        match v {
            Value::Int(n) => i32::try_from(*n).map_err(|_| "expected int32".into()),
            _ => Err("expected int32".into()),
        }
    }
    fn text(v: &Value) -> Result<String, String> {
        Interpreter::expect_string_or_format("socket endpoint", v.clone())
            .map_err(|e| e.to_string())
    }
    fn bytes(v: &Value) -> Result<Vec<u8>, String> {
        let Value::Slice(s) = v else {
            return Err("expected []uint8".into());
        };
        let items = s.items.lock().unwrap();
        if items.len() > net::MAX_BYTES {
            return Err("payload exceeds 65535 bytes".into());
        }
        items
            .iter()
            .map(|v| u8::try_from(Self::integer(v)?).map_err(|_| "expected byte".into()))
            .collect()
    }
    fn replace(v: &Value, data: Vec<u8>) -> Result<(), String> {
        let Value::Slice(s) = v else {
            return Err("expected output slice".into());
        };
        *s.items.lock().unwrap() = data.into_iter().map(|b| Value::Int(b.into())).collect();
        Ok(())
    }
}
impl Interpreter {
    pub(super) fn builtin_socket(&mut self, op: Op, args: &[Value]) -> RuntimeResult<Vec<Value>> {
        let (variant, value) = match net::invoke::<Interpreted>(&self.network, op, args) {
            Ok(Reply::Unit) => ("Ok", Value::Unit),
            Ok(Reply::Text(s)) => ("Ok", Value::String(s)),
            Ok(Reply::Int(n)) => ("Ok", Value::Int(n.into())),
            Err(s) => ("Err", Value::String(s)),
            _ => unreachable!(),
        };
        Ok(vec![self.instantiate_enum(
            "Result",
            variant,
            vec![value],
        )?])
    }
}
