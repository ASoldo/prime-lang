//! Async runtime scaffolding for async/await support.
//! This file seeds a cooperative scheduler; until all primitives are non-blocking,
//! tasks still execute on helper threads to preserve correctness.

use crate::runtime::{
    error::RuntimeResult,
    value::{TaskState, TaskValue, Value},
};
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

#[derive(Clone, Default)]
pub struct AsyncRuntime {
    shared: Arc<AsyncShared>,
}

#[derive(Default)]
struct AsyncShared {
    ready: Mutex<VecDeque<TaskHandle>>,
}

struct TaskHandle {
    state: Arc<(Mutex<TaskState>, std::sync::Condvar)>,
    deadline: Option<Instant>,
    runnable: Option<Box<dyn FnOnce() -> RuntimeResult<Value> + Send>>,
}

impl AsyncRuntime {
    pub fn new() -> Self {
        Self {
            shared: Arc::new(AsyncShared {
                ready: Mutex::new(VecDeque::new()),
            }),
        }
    }

    /// Spawn a task and return a TaskValue handle. Currently drives runnables cooperatively;
    /// once timers/channels are non-blocking this will be pumped by the scheduler loop.
    pub fn spawn<F>(&self, f: F) -> TaskValue
    where
        F: FnOnce() -> RuntimeResult<Value> + Send + 'static,
    {
        let (task, state) = TaskValue::new_pair();
        self.enqueue(TaskHandle {
            state: state.clone(),
            deadline: None,
            runnable: Some(Box::new(f)),
        });
        self.drive_ready();
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
        let duration = if millis <= 0 {
            Duration::from_millis(0)
        } else {
            Duration::from_millis(millis as u64)
        };
        let deadline = Instant::now() + duration;
        self.enqueue(TaskHandle {
            state: state.clone(),
            deadline: Some(deadline),
            runnable: None,
        });
        // Spawn a timer thread to wake the task when due. This will be replaced by timer wheel.
        std::thread::spawn(move || {
            std::thread::sleep(duration);
            TaskValue::store_result(&state, Ok(Value::Unit));
        });
        task
    }

    /// Cooperative tick: pop any ready task whose deadline has passed or finished.
    #[allow(dead_code)]
    pub fn poll_ready(&self) -> Option<TaskValue> {
        let mut ready = self.shared.ready.lock().ok()?;
        let now = Instant::now();
        let pos = ready
            .iter()
            .position(|handle| handle.deadline.map(|d| d <= now).unwrap_or(true))
            .or_else(|| ready.iter().position(|handle| handle.state.0.lock().map(|g| g.finished).unwrap_or(false)));
        if let Some(idx) = pos {
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
        self.enqueue(TaskHandle {
            state,
            deadline: Some(deadline),
            runnable: None,
        });
    }

    fn enqueue(&self, handle: TaskHandle) {
        let mut ready = self.shared.ready.lock().unwrap();
        ready.push_back(handle);
    }

    fn next_deadline(&self) -> Option<Instant> {
        let ready = self.shared.ready.lock().ok()?;
        ready.iter().filter_map(|h| h.deadline).min()
    }

    fn drive_ready(&self) {
        loop {
            let maybe_handle = {
                let mut ready = self.shared.ready.lock().unwrap();
                let now = Instant::now();
                let pos = ready.iter().position(|h| h.deadline.map(|d| d <= now).unwrap_or(true));
                match pos {
                    Some(idx) => Some(ready.remove(idx).unwrap()),
                    None => ready.pop_front(),
                }
            };
            let Some(mut handle) = maybe_handle else { break };
            if let Some(runnable) = handle.runnable.take() {
                let state = handle.state.clone();
                let result = runnable();
                TaskValue::store_result(&state, result);
            } else {
                // not runnable yet (timer/channel) — if it's a sleep, leave it queued
                self.enqueue(handle);
                break;
            }
        }
    }

    /// Block until the given task completes by draining the ready queue cooperatively.
    pub fn block_on(&self, task: &TaskValue) -> RuntimeResult<Value> {
        // Fast path: already done.
        if task.is_finished() {
            return task.join().map_err(|msg| crate::runtime::error::RuntimeError::Panic { message: msg });
        }
        // Drive until finished.
        let start = Instant::now();
        loop {
            self.drive_ready();
            if task.is_finished() {
                return task.join().map_err(|msg| crate::runtime::error::RuntimeError::Panic { message: msg });
            }
            if let Some(deadline) = self.next_deadline() {
                let now = Instant::now();
                if deadline > now {
                    let sleep_dur = deadline - now;
                    std::thread::sleep(sleep_dur);
                } else {
                    std::thread::yield_now();
                }
            } else {
                // Avoid busy loop; in the future this will block on pollers.
                std::thread::sleep(Duration::from_millis(1));
            }
            if start.elapsed() > Duration::from_secs(1) {
                break;
            }
        }
        Err(crate::runtime::error::RuntimeError::Unsupported {
            message: "async task did not finish; cooperative scheduler needs wakeups (timers/channels)".into(),
        })
    }

}
