//! Versioned records for the interpreter's stderr trace.
use serde::Serialize;
use std::io::{self, Write};

#[derive(Serialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum TraceEvent<'a> {
    Call {
        module: &'a str,
        function: &'a str,
        path: &'a str,
        depth: usize,
        start: usize,
        end: usize,
    },
    RuntimeError {
        message: &'a str,
    },
    Warning {
        message: &'a str,
    },
}

impl TraceEvent<'_> {
    pub fn emit(self) {
        #[derive(Serialize)]
        struct Record<'a> {
            schema_version: u8,
            #[serde(flatten)]
            event: TraceEvent<'a>,
        }

        let mut bytes = serde_json::to_vec(&Record {
            schema_version: 1,
            event: self,
        })
        .expect("trace records contain only strings and integers");
        bytes.push(b'\n');
        // A closed trace consumer must not interrupt the Prime program.
        let _ = io::stderr().lock().write_all(&bytes);
    }
}
