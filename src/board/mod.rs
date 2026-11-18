use clap::Subcommand;
use prime_lang::target::{ESP32_XTENSA_ESPIDF_TRIPLE, ESP32C3_TRIPLE};
use serde::{Deserialize, Serialize};
use std::{
    env, fs, io,
    io::Write,
    num::NonZeroU32,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};
use tempfile::NamedTempFile;

#[derive(Debug, Subcommand)]
pub enum BoardCommand {
    /// Detect the chip and flash geometry on the selected serial port
    Inspect {
        #[arg(long, value_name = "PORT")]
        port: String,
    },
    /// Read and verify all detected SPI NOR flash, then publish a new image
    Backup {
        #[arg(long, value_name = "PORT")]
        port: String,
        #[arg(long, value_name = "NEW_FILE")]
        out: PathBuf,
        /// Transfer rate after the initial serial handshake
        #[arg(long, default_value = "460800")]
        baud: NonZeroU32,
    },
}

#[derive(Deserialize, Serialize)]
struct BoardReport {
    schema_version: u8,
    port: String,
    chip: String,
    description: String,
    mac: String,
    flash_bytes: u64,
    esptool_version: String,
    prime_target: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    image: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    sha256: Option<String>,
}

pub fn run(command: BoardCommand) -> Result<(), String> {
    let python = env::var_os("PRIME_ESP_PYTHON").unwrap_or_else(|| "python3".into());
    let mut sdk = Command::new(&python);
    sdk.args(["-I", "-c", include_str!("espressif.py")])
        .stdin(Stdio::null())
        .stderr(Stdio::inherit());
    let backup = match command {
        BoardCommand::Inspect { port } => {
            sdk.arg(port);
            None
        }
        BoardCommand::Backup { port, out, baud } => {
            if fs::symlink_metadata(&out).is_ok() {
                return Err(format!("output already exists: {}", out.display()));
            }
            let parent = out.parent().filter(|p| !p.as_os_str().is_empty());
            let image = NamedTempFile::new_in(parent.unwrap_or_else(|| Path::new(".")))
                .map_err(|err| format!("cannot prepare {}: {err}", out.display()))?;
            sdk.arg(port).arg(image.path()).arg(baud.to_string());
            Some((image, out))
        }
    };
    let result = sdk.output().map_err(|err| {
        format!(
            "could not start {}: {err} (set PRIME_ESP_PYTHON to the SDK interpreter)",
            python.to_string_lossy()
        )
    })?;
    if !result.status.success() {
        return Err(format!("Espressif probe exited with {}", result.status));
    }
    let mut report: BoardReport = serde_json::from_slice(&result.stdout)
        .map_err(|err| format!("invalid SDK report: {err}"))?;
    report.prime_target = match report.chip.as_str() {
        "ESP32" => Some(ESP32_XTENSA_ESPIDF_TRIPLE.into()),
        "ESP32-C3" => Some(ESP32C3_TRIPLE.into()),
        _ => None,
    };
    if let Some((image, out)) = backup {
        let file = image.as_file();
        let length = file.metadata().map_err(|err| err.to_string())?.len();
        if length == 0 || report.flash_bytes != length || report.sha256.is_none() {
            return Err("incomplete verified flash image".into());
        }
        file.sync_all()
            .map_err(|err| format!("cannot finalize flash image: {err}"))?;
        image
            .persist_noclobber(&out)
            .map_err(|err| format!("cannot save {}: {}", out.display(), err.error))?;
        report.image = Some(out.to_string_lossy().into_owned());
    }
    let mut stdout = io::stdout().lock();
    serde_json::to_writer_pretty(&mut stdout, &report).map_err(|err| err.to_string())?;
    writeln!(stdout).map_err(|err| err.to_string())
}
