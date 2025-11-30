//! Async runtime scaffolding for async/await support.
//! Cooperative async scheduler used by the interpreter and host runtime ABI.

use crate::runtime::{
    error::RuntimeResult,
    value::{TaskState, TaskValue, TryRecvOutcome, Value, make_option_value},
};
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

fn debug_enabled() -> bool {
    std::env::var_os("PRIME_DEBUG_ASYNC").is_some()
}

#[derive(Clone, Default)]
pub struct AsyncRuntime {
    shared: Arc<AsyncShared>,
}

#[derive(Default)]
struct AsyncShared {
    ready: Mutex<VecDeque<TaskHandle>>,
    sleepers: Mutex<Vec<TimerHandle>>,
    channel_waiters: Mutex<Vec<ChannelWaiter>>,
}

struct TaskHandle {
    state: Arc<(Mutex<TaskState>, std::sync::Condvar)>,
    runnable: Option<Box<dyn FnOnce() -> RuntimeResult<Value> + Send>>,
}

struct TimerHandle {
    state: Arc<(Mutex<TaskState>, std::sync::Condvar)>,
    deadline: Instant,
}

struct ChannelWaiter {
    state: Arc<(Mutex<TaskState>, std::sync::Condvar)>,
    receiver: crate::runtime::value::ChannelReceiver,
}

impl AsyncRuntime {
    pub fn new() -> Self {
        Self {
            shared: Arc::new(AsyncShared {
                ready: Mutex::new(VecDeque::new()),
                sleepers: Mutex::new(Vec::new()),
                channel_waiters: Mutex::new(Vec::new()),
            }),
        }
    }

    /// Spawn a task and return a TaskValue handle. Runnables execute cooperatively on the
    /// driving thread; async waits are advanced via timers and channel polls.
    pub fn spawn<F>(&self, f: F) -> TaskValue
    where
        F: FnOnce() -> RuntimeResult<Value> + Send + 'static,
    {
        let (task, state) = TaskValue::new_pair();
        if debug_enabled() {
            eprintln!("[prime-debug] async_runtime spawn task");
        }
        self.enqueue(TaskHandle {
            state: state.clone(),
            runnable: Some(Box::new(f)),
        });
        task
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
        let (task, state) = TaskValue::new_pair();
        let duration = Duration::from_millis(millis.max(0) as u64);
        let deadline = Instant::now() + duration;
        let mut sleepers = self.shared.sleepers.lock().unwrap();
        sleepers.push(TimerHandle { state, deadline });
        drop(sleepers);
        self.drive_ready();
        task
    }

    /// Create a task that resolves when the channel yields a value or closes.
    pub fn recv_task(&self, receiver: crate::runtime::value::ChannelReceiver) -> TaskValue {
        let (task, state) = TaskValue::new_pair();
        if debug_enabled() {
            eprintln!("[prime-debug] async_runtime recv_task registered");
        }
        let mut waiters = self.shared.channel_waiters.lock().unwrap();
        waiters.push(ChannelWaiter { state, receiver });
        drop(waiters);
        self.drive_ready();
        task
    }

    /// Cooperative tick: pop any ready task whose deadline has passed or finished.
    #[allow(dead_code)]
    pub fn poll_ready(&self) -> Option<TaskValue> {
        let mut ready = self.shared.ready.lock().ok()?;
        if let Some(idx) = ready
            .iter()
            .position(|handle| handle.state.0.lock().map(|g| g.finished).unwrap_or(false))
        {
            let handle = ready.remove(idx)?;
            return Some(TaskValue::with_state(handle.state));
        }
        None
    }

    /// Register a timer wakeup for the current task (stubbed for now).
    #[allow(dead_code)]
    pub fn sleep_until(
        &self,
        state: Arc<(Mutex<TaskState>, std::sync::Condvar)>,
        deadline: Instant,
    ) {
        let mut sleepers = self.shared.sleepers.lock().unwrap();
        sleepers.push(TimerHandle { state, deadline });
    }

    fn enqueue(&self, handle: TaskHandle) {
        let mut ready = self.shared.ready.lock().unwrap();
        ready.push_back(handle);
    }

    fn next_deadline(&self) -> Option<Instant> {
        let sleepers = self.shared.sleepers.lock().ok()?;
        sleepers.iter().map(|h| h.deadline).min()
    }

    fn drive_ready(&self) {
        loop {
            let mut progressed = false;
            progressed |= self.run_ready();
            progressed |= self.poll_sleepers();
            progressed |= self.poll_channels();
            if !progressed {
                break;
            }
        }
    }

    fn run_ready(&self) -> bool {
        let maybe_handle = {
            let mut ready = self.shared.ready.lock().unwrap();
            ready.pop_front()
        };
        let Some(mut handle) = maybe_handle else {
            return false;
        };
        if let Some(runnable) = handle.runnable.take() {
            let state = handle.state.clone();
            let result = runnable();
            TaskValue::store_result(&state, result);
            true
        } else {
            false
        }
    }

    fn poll_sleepers(&self) -> bool {
        let now = Instant::now();
        let mut sleepers = self.shared.sleepers.lock().unwrap();
        let mut progressed = false;
        let mut i = 0;
        while i < sleepers.len() {
            if sleepers[i].deadline <= now {
                let handle = sleepers.remove(i);
                TaskValue::store_result(&handle.state, Ok(Value::Unit));
                progressed = true;
            } else {
                i += 1;
            }
        }
        progressed
    }

    fn poll_channels(&self) -> bool {
        let mut waiters = self.shared.channel_waiters.lock().unwrap();
        let mut progressed = false;
        let mut i = 0;
        while i < waiters.len() {
            match waiters[i].receiver.try_recv() {
                TryRecvOutcome::Item(v) => {
                    let handle = waiters.remove(i);
                    TaskValue::store_result(&handle.state, Ok(make_option_value(Some(v))));
                    if debug_enabled() {
                        eprintln!("[prime-debug] async_runtime channel woke with item");
                    }
                    progressed = true;
                }
                TryRecvOutcome::Closed => {
                    let handle = waiters.remove(i);
                    TaskValue::store_result(&handle.state, Ok(make_option_value(None)));
                    if debug_enabled() {
                        eprintln!("[prime-debug] async_runtime channel closed wake");
                    }
                    progressed = true;
                }
                TryRecvOutcome::Pending => {
                    i += 1;
                }
            }
        }
        progressed
    }

    /// Block until the given task completes by draining the ready queue cooperatively.
    pub fn block_on(&self, task: &TaskValue) -> RuntimeResult<Value> {
        // Fast path: already done.
        if task.is_finished() {
            return task
                .join()
                .map_err(|msg| crate::runtime::error::RuntimeError::Panic { message: msg });
        }
        // Drive until finished.
        loop {
            self.drive_ready();
            if task.is_finished() {
                if debug_enabled() {
                    eprintln!("[prime-debug] async_runtime task finished");
                }
                return task
                    .join()
                    .map_err(|msg| crate::runtime::error::RuntimeError::Panic { message: msg });
            }
            if let Some(deadline) = self.next_deadline() {
                let now = Instant::now();
                if deadline > now {
                    std::thread::sleep(deadline - now);
                }
            } else {
                std::thread::sleep(Duration::from_millis(1));
            }
            if debug_enabled() {
                eprintln!("[prime-debug] async_runtime block_on tick");
            }
        }
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
