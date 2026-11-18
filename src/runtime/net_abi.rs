// Included inside abi::host to use the native Prime value representation.
use super::super::net::{self, Network, Op, Reply, Values};
use super::*;
struct Native;

impl Values for Native {
    type Value = PrimeHandle;
    fn integer(v: &PrimeHandle) -> Result<i32, String> {
        match unsafe { clone_value(*v) } {
            HostValue::Int(n) => i32::try_from(n).map_err(|_| "expected int32".into()),
            _ => Err("expected int32".into()),
        }
    }
    fn text(v: &PrimeHandle) -> Result<String, String> {
        unsafe { handle_string(v).map(str::to_owned) }
    }
    fn bytes(v: &PrimeHandle) -> Result<Vec<u8>, String> {
        let HostValue::Slice(items) = (unsafe { clone_value(*v) }) else {
            return Err("expected []uint8".into());
        };
        let items = items.lock().unwrap();
        if items.len() > net::MAX_BYTES {
            return Err("payload exceeds 65535 bytes".into());
        }
        items
            .iter()
            .map(|v| u8::try_from(Self::integer(v)?).map_err(|_| "expected byte".into()))
            .collect()
    }
    fn replace(v: &PrimeHandle, data: Vec<u8>) -> Result<(), String> {
        let HostValue::Slice(items) = (unsafe { clone_value(*v) }) else {
            return Err("expected output slice".into());
        };
        let new = data
            .into_iter()
            .map(|b| to_handle(HostHandle::new(HostValue::Int(b.into()))))
            .collect();
        let old = std::mem::replace(&mut *items.lock().unwrap(), new);
        for v in old {
            unsafe {
                prime_value_release(v);
            }
        }
        Ok(())
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn prime_net_call(
    op: u32,
    a: PrimeHandle,
    b: PrimeHandle,
    c: PrimeHandle,
    d: PrimeHandle,
    ok: u32,
    err: u32,
) -> PrimeHandle {
    static NET: std::sync::LazyLock<Network> = std::sync::LazyLock::new(Network::default);
    let result = Op::ALL
        .get(op as usize)
        .ok_or_else(|| "invalid socket operation".to_owned())
        .and_then(|&op| {
            let args = [a, b, c, d];
            net::invoke::<Native>(&NET, op, &args[..op.parameters().len()])
        })
        .map(|v| match v {
            Reply::Int(n) => HostValue::Int(n.into()),
            Reply::Text(s) => HostValue::Str(s),
            Reply::Unit => HostValue::Unit,
            _ => unreachable!(),
        });
    result_handle(result, ok, err)
}
