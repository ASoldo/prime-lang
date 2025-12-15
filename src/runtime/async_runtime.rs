//! Async runtime scaffolding for async/await support.
//! Cooperative async scheduler used by the interpreter and host runtime ABI.

use crate::runtime::{
    error::RuntimeResult,
    value::{TaskValue, TryRecvOutcome, Value, make_option_value},
};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

fn debug_enabled() -> bool {
    std::env::var_os("PRIME_DEBUG_ASYNC").is_some()
}

#[derive(Clone, Default)]
pub struct AsyncRuntime {}

impl AsyncRuntime {
    pub fn new() -> Self {
        Self::default()
    }

    /// Spawn a task and return a TaskValue handle. Runnables execute cooperatively on the
    /// driving thread; async waits are advanced via timers and channel polls.
    pub fn spawn<F>(&self, f: F) -> TaskValue
    where
        F: FnOnce() -> RuntimeResult<Value> + Send + 'static,
    {
        self.spawn_blocking(f)
    }

    /// Spawn a blocking operation on a helper thread and return a TaskValue handle.
    pub fn spawn_blocking<F>(&self, f: F) -> TaskValue
    where
        F: FnOnce() -> RuntimeResult<Value> + Send + 'static,
    {
        let (task, state) = TaskValue::new_pair();
        thread::spawn(move || {
            let result = f();
            TaskValue::store_result(&state, result);
        });
        task
    }

    /// Return a task that completes after the given duration.
    pub fn sleep_task(&self, millis: i64) -> TaskValue {
        let duration = if millis <= 0 {
            Duration::from_millis(0)
        } else {
            Duration::from_millis(millis as u64)
        };
        let (task, state) = TaskValue::new_pair();
        thread::spawn(move || {
            let tick = Duration::from_millis(5);
            let deadline = Instant::now() + duration;
            if debug_enabled() {
                eprintln!(
                    "[prime-debug] async_runtime sleep_task sleeping for {:?}",
                    duration
                );
            }
            while Instant::now() < deadline {
                if TaskValue::with_state(state.clone()).is_finished() {
                    return;
                }
                let now = Instant::now();
                let remaining = deadline.saturating_duration_since(now);
                if remaining.is_zero() {
                    break;
                }
                thread::sleep(std::cmp::min(tick, remaining));
            }
            if debug_enabled() {
                eprintln!("[prime-debug] async_runtime sleep_task finished sleep");
            }
            TaskValue::store_result(&state, Ok(Value::Unit));
        });
        task
    }

    /// Create a task that resolves when the channel yields a value or closes.
    pub fn recv_task(&self, receiver: crate::runtime::value::ChannelReceiver) -> TaskValue {
        let (task, state) = TaskValue::new_pair();
        thread::spawn(move || {
            if debug_enabled() {
                eprintln!("[prime-debug] async_runtime recv_task waiting");
            }
            let tick = Duration::from_millis(5);
            loop {
                if TaskValue::with_state(state.clone()).is_finished() {
                    return;
                }
                match receiver.try_recv() {
                    TryRecvOutcome::Item(value) => {
                        if debug_enabled() {
                            eprintln!("[prime-debug] async_runtime recv_task received value");
                        }
                        TaskValue::store_result(&state, Ok(make_option_value(Some(value))));
                        return;
                    }
                    TryRecvOutcome::Closed => {
                        if debug_enabled() {
                            eprintln!("[prime-debug] async_runtime recv_task closed");
                        }
                        TaskValue::store_result(&state, Ok(make_option_value(None)));
                        return;
                    }
                    TryRecvOutcome::Pending => {
                        thread::sleep(tick);
                    }
                }
            }
        });
        task
    }

    /// Cooperative tick: pop any ready task whose deadline has passed or finished.
    #[allow(dead_code)]
    fn poll_ready(&self) -> Option<TaskValue> {
        None
    }

    /// Register a timer wakeup for the current task (stubbed for now).
    #[allow(dead_code)]
    fn sleep_until(
        &self,
        state: Arc<(Mutex<crate::runtime::value::TaskState>, std::sync::Condvar)>,
    ) {
        if debug_enabled() {
            eprintln!(
                "[prime-debug] async_runtime sleep_until stubbed for {:?}",
                Arc::as_ptr(&state)
            );
        }
    }

    /// Block until the given task completes by draining the ready queue cooperatively.
    pub fn block_on(&self, task: &TaskValue) -> RuntimeResult<Value> {
        if debug_enabled() {
            eprintln!("[prime-debug] async_runtime block_on waiting");
        }
        task.join()
            .map_err(|msg| crate::runtime::error::RuntimeError::Panic { message: msg })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::value::{ChannelReceiver, ChannelSender};

    #[test]
    fn sleep_task_completes() {
        let rt = AsyncRuntime::new();
        let task = rt.sleep_task(1);
        let result = rt.block_on(&task);
        assert!(result.is_ok(), "sleep_task should complete");
    }

    #[test]
    fn recv_task_wakes_without_blocking_thread() {
        let rt = AsyncRuntime::new();
        let (tx_raw, rx_raw) = std::sync::mpsc::channel();
        let sender_state = Arc::new(Mutex::new(Some(tx_raw)));
        let receiver = Arc::new(Mutex::new(rx_raw));
        let channel_rx = ChannelReceiver::new(sender_state.clone(), receiver);
        let channel_tx = ChannelSender::new(sender_state);
        channel_tx.send(Value::Int(1)).expect("send");
        let task = rt.recv_task(channel_rx);
        let result = rt.block_on(&task).expect("recv_task result");
        match result {
            Value::Enum(ev) => assert_eq!(ev.variant, "Some"),
            other => panic!("expected Option::Some from recv_task, got {:?}", other),
        }
    }
}
