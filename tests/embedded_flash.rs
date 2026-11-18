#![cfg(unix)]

use prime_lang::target::{ESP32_XTENSA_ESPIDF_TRIPLE, ESP32_XTENSA_TRIPLE, ESP32C3_TRIPLE};
use std::{fs, os::unix::fs::PermissionsExt, path::Path, process::Command};
use tempfile::TempDir;

struct Project {
    dir: TempDir,
    elf: String,
    bin: String,
}

fn script(path: &Path, body: &str) {
    fs::write(path, format!("#!/bin/sh\nset -eu\n{body}\n")).unwrap();
    fs::set_permissions(path, fs::Permissions::from_mode(0o755)).unwrap();
}

impl Project {
    fn new(triple: &str, flash: &str) -> Self {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        fs::write(root.join("main.prime"), "module demo;\nfn main() {}\n").unwrap();
        fs::write(
            root.join("prime.toml"),
            format!(
                r#"
manifest_version = "3"
[package]
name = "demo"
version = "0.1.0"
[module]
name = "demo"
path = "main.prime"
no_std = true
[build]
target = "{triple}"
[build.toolchain]
cc = "mock-cc"
ld_script = "mock.ld"
objcopy = "mock-objcopy"
[build.flash]
enabled = true
{flash}
"#
            ),
        )
        .unwrap();
        // Isolate the packaging CLI from cross compilers, serial ports and the user's IDF tree.
        let tools = root.join("tools");
        fs::create_dir(&tools).unwrap();
        script(
            &tools.join("mock-cc"),
            r#"while [ "$1" != -o ]; do shift; done
printf 'fixture' > "$2""#,
        );
        script(
            &tools.join("cargo"),
            r#"while [ "$#" -gt 0 ]; do
  case "$1" in
    --target) triple=$2; shift ;;
    --target-dir) target_dir=$2; shift ;;
  esac
  shift
done
mkdir -p "$target_dir/$triple/release"
: > "$target_dir/$triple/release/libruntime_abi.a""#,
        );
        script(&tools.join("mock-objcopy"), r#"printf 'raw binary' > "$4""#);
        script(
            &tools.join("esptool"),
            r#"printf '%s\n' "$@" '' >> calls
for arg do
  case "$arg" in
    write-flash) exit 0 ;;
    elf2image)
      while [ "$1" != -o ]; do shift; done
      case "${MOCK_CONVERSION:-ok}" in
        missing) exit 0 ;;
        empty) : > "$2" ;;
        fail) echo 'conversion failed' >&2; exit 3 ;;
        ok) printf 'converted image' > "$2" ;;
      esac
      exit 0 ;;
  esac
done
exit 2"#,
        );
        let elf = format!(".build.prime/demo-{triple}/demo");
        let bin = format!("{elf}.bin");
        Self { dir, elf, bin }
    }

    fn build(&self, args: &[&str], vars: &[(&str, &str)]) -> Result<(), String> {
        let root = self.dir.path();
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_prime-lang"));
        cmd.current_dir(root).env_clear();
        for key in ["LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH"] {
            if let Some(value) = std::env::var_os(key) {
                cmd.env(key, value);
            }
        }
        let output = cmd
            .env("HOME", root)
            .env("IDF_PATH", root)
            .env("PATH", format!("{}/tools:/usr/bin:/bin", root.display()))
            .env("PRIME_LLC", root.join("tools/mock-cc"))
            .env("PRIME_ESPTOOL", root.join("tools/esptool"))
            .envs(vars.iter().copied())
            .args(["build", "main.prime", "--name", "demo"])
            .args(args)
            .output()
            .unwrap();
        if output.status.success() {
            Ok(())
        } else {
            Err(String::from_utf8(output.stderr).unwrap())
        }
    }

    fn calls(&self) -> Vec<Vec<String>> {
        let text = fs::read_to_string(self.dir.path().join("calls")).unwrap();
        text.split_terminator("\n\n")
            .map(|call| call.lines().map(str::to_owned).collect())
            .collect()
    }

    fn conversion(&self, chip: &'static str) -> Vec<&str> {
        vec![
            "--chip",
            chip,
            "elf2image",
            "-fm",
            "dio",
            "-ff",
            "40m",
            "-fs",
            "4MB",
            "-o",
            &self.bin,
            &self.elf,
        ]
    }

    fn assert_write(&self, chip: &'static str, serial_options: &[&str], images: &[&str]) {
        let mut write = vec!["--chip", chip];
        write.extend_from_slice(serial_options);
        write.extend(["write-flash", "-z"]);
        write.extend_from_slice(images);
        assert_eq!(self.calls(), [self.conversion(chip), write]);
    }
}

#[test]
fn app_only_ignores_idf_examples_and_respects_the_manifest() {
    for (triple, chip) in [
        (ESP32_XTENSA_ESPIDF_TRIPLE, "esp32"),
        (ESP32C3_TRIPLE, "esp32c3"),
    ] {
        let project = Project::new(
            triple,
            "address = \"131072\"\nport = \"mock port\"\nbaud = 921600",
        );
        let blink = project
            .dir
            .path()
            .join("esp/esp-idf/examples/get-started/blink/build");
        for file in [
            "bootloader/bootloader.bin",
            "partition_table/partition-table.bin",
        ] {
            let path = blink.join(file);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, b"unrelated").unwrap();
        }
        project
            .build(
                &[],
                &[
                    ("PRIME_ESP_PORT", "ignored"),
                    ("ESPTOOL_PORT", "ignored"),
                    ("PRIME_ESP_BAUD", "9600"),
                ],
            )
            .unwrap();
        project.assert_write(
            chip,
            &["--port", "mock port", "--baud", "921600"],
            &["0x20000", &project.bin],
        );
    }
}

#[test]
fn explicit_components_get_chip_specific_offsets() {
    for (triple, chip, boot_offset) in [
        (ESP32_XTENSA_TRIPLE, "esp32", "0x1000"),
        (ESP32C3_TRIPLE, "esp32c3", "0x0"),
    ] {
        let project = Project::new(triple, "");
        let boot = project.dir.path().join("boot loader.bin");
        let table = project.dir.path().join("table.bin");
        fs::write(&boot, b"boot").unwrap();
        fs::write(&table, b"partitions").unwrap();
        project
            .build(
                &[],
                &[
                    ("PRIME_ESP_PORT", "mock prime port"),
                    ("PRIME_ESP_BAUD", "230400"),
                    ("PRIME_ESP_BOOTLOADER", "boot loader.bin"),
                    ("PRIME_ESP_PARTITIONS", "table.bin"),
                ],
            )
            .unwrap();
        project.assert_write(
            chip,
            &["--port", "mock prime port", "--baud", "230400"],
            &[
                boot_offset,
                boot.to_str().unwrap(),
                "0x8000",
                table.to_str().unwrap(),
                "0x10000",
                &project.bin,
            ],
        );
    }
}

#[test]
fn each_explicit_component_can_be_written_independently_with_native_port_selection() {
    for (key, offset, native_port) in [
        ("PRIME_ESP_BOOTLOADER", "0x0", None),
        ("PRIME_ESP_PARTITIONS", "0x8000", Some("mock native port")),
    ] {
        let project = Project::new(ESP32C3_TRIPLE, "");
        let file = project.dir.path().join("component.bin");
        fs::write(&file, b"component").unwrap();
        let mut vars = vec![(key, "component.bin")];
        if let Some(port) = native_port {
            vars.push(("ESPTOOL_PORT", port));
        }
        project.build(&[], &vars).unwrap();
        project.assert_write(
            "esp32c3",
            &["--baud", "460800"],
            &[offset, file.to_str().unwrap(), "0x10000", &project.bin],
        );
    }
}

#[test]
fn component_and_address_errors_stop_before_write_flash() {
    let project = Project::new(ESP32_XTENSA_TRIPLE, "");
    fs::write(project.dir.path().join("empty"), []).unwrap();
    fs::write(project.dir.path().join("large"), vec![1; 0x7001]).unwrap();
    let cases = [
        ("PRIME_ESP_BOOTLOADER", "missing"),
        ("PRIME_ESP_PARTITIONS", ""),
        ("PRIME_ESP_BOOTLOADER", "tools"),
        ("PRIME_ESP_PARTITIONS", "empty"),
        ("PRIME_ESP_BOOTLOADER", "large"),
        ("PRIME_ESP_PARTITIONS", "large"),
    ];
    for (key, path) in cases {
        let err = project.build(&[], &[(key, path)]).unwrap_err();
        assert!(err.contains(key) && err.contains(path), "{err}");
    }
    assert_eq!(
        project.calls(),
        vec![project.conversion("esp32"); cases.len()]
    );
    for address in [
        "oops",
        "0",
        "0x1000",
        "0x8000",
        "0x10001",
        "0x1000000",
        "0x100000000",
    ] {
        let project = Project::new(ESP32C3_TRIPLE, &format!("address = '{address}'"));
        let err = project.build(&[], &[]).unwrap_err();
        assert!(err.contains(address), "{err}");
        assert_eq!(project.calls(), [project.conversion("esp32c3")]);
    }
}

#[test]
fn stale_or_empty_conversion_output_cannot_be_flashed() {
    for mode in ["missing", "empty", "fail"] {
        let project = Project::new(ESP32C3_TRIPLE, "");
        let bin = project.dir.path().join(&project.bin);
        fs::create_dir_all(bin.parent().unwrap()).unwrap();
        fs::write(bin, b"stale application").unwrap();
        let err = project
            .build(&[], &[("MOCK_CONVERSION", mode)])
            .unwrap_err();
        assert!(err.contains("generate a flashable image"), "{err}");
        assert_eq!(project.calls(), [project.conversion("esp32c3")]);
    }
}

#[test]
fn no_flash_builds_images_without_consulting_write_settings() {
    for (mode, expected) in [("ok", "converted image"), ("fail", "raw binary")] {
        let project = Project::new(ESP32C3_TRIPLE, "address = 'invalid'");
        project
            .build(
                &["--no-flash"],
                &[
                    ("PRIME_ESP_PARTITIONS", "missing"),
                    ("MOCK_CONVERSION", mode),
                ],
            )
            .unwrap();
        assert_eq!(project.calls(), [project.conversion("esp32c3")]);
        assert_eq!(
            fs::read_to_string(project.dir.path().join(&project.bin)).unwrap(),
            expected
        );
    }
}
