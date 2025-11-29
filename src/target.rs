use std::{env, mem};

/// Default target for host builds.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuildTarget {
    Host,
    Triple(String),
}

pub const ESP32C3_TRIPLE: &str = "riscv32imc-unknown-none-elf";
pub const ESP32_XTENSA_TRIPLE: &str = "xtensa-esp32-none-elf";
pub const ESP32_XTENSA_ESPIDF_TRIPLE: &str = "xtensa-esp32-espidf";

impl BuildTarget {
    pub fn host() -> Self {
        Self::Host
    }

    pub fn from_flag_or_env(flag: Option<String>) -> Self {
        let target = flag
            .or_else(|| env::var("PRIME_TARGET").ok())
            .filter(|t| !t.trim().is_empty());
        match target {
            Some(triple) => Self::Triple(triple),
            None => Self::Host,
        }
    }

    pub fn triple(&self) -> Option<&str> {
        match self {
            BuildTarget::Host => None,
            BuildTarget::Triple(triple) => Some(triple),
        }
    }

    #[allow(dead_code)]
    pub fn is_host(&self) -> bool {
        matches!(self, BuildTarget::Host)
    }

    pub fn is_esp32c3(&self) -> bool {
        matches!(self, BuildTarget::Triple(triple) if triple == ESP32C3_TRIPLE)
    }

    pub fn is_esp32_xtensa(&self) -> bool {
        matches!(self, BuildTarget::Triple(triple) if triple == ESP32_XTENSA_TRIPLE)
    }

    pub fn is_esp32_xtensa_espidf(&self) -> bool {
        matches!(self, BuildTarget::Triple(triple) if triple == ESP32_XTENSA_ESPIDF_TRIPLE)
    }

    pub fn is_embedded(&self) -> bool {
        self.is_esp32c3() || self.is_esp32_xtensa() || self.is_esp32_xtensa_espidf()
    }

    pub fn pointer_width_bits(&self) -> u32 {
        if self.is_embedded() {
            32
        } else {
            (mem::size_of::<usize>() * 8) as u32
        }
    }
}

pub fn embedded_target_hint() -> String {
    format!("{}, {}, {}", ESP32C3_TRIPLE, ESP32_XTENSA_TRIPLE, ESP32_XTENSA_ESPIDF_TRIPLE)
}

#[derive(Clone, Debug, Default)]
pub struct ToolchainSettings {
    pub cc: Option<String>,
    #[allow(dead_code)]
    pub ar: Option<String>,
    pub objcopy: Option<String>,
    pub ld_script: Option<String>,
    pub startup_obj: Option<String>,
    pub ld_flags: Option<String>,
    pub esptool: Option<String>,
    pub env: Option<std::collections::HashMap<String, String>>,
}

#[derive(Clone, Debug)]
pub struct BuildOptions {
    pub target: BuildTarget,
    pub platform: Option<String>,
    pub toolchain: ToolchainSettings,
    pub flash: FlashSettings,
}

impl BuildOptions {
    pub fn from_sources(
        target_flag: Option<String>,
        platform_flag: Option<String>,
        manifest: Option<&crate::project::manifest::PackageManifest>,
    ) -> Self {
        let manifest_build = manifest.and_then(|m| m.build_settings());
        let target = match target_flag
            .clone()
            .or_else(|| manifest_build.and_then(|b| b.target.clone()))
        {
            Some(triple) => BuildTarget::Triple(triple),
            None => BuildTarget::from_flag_or_env(None),
        };
        let platform = platform_flag
            .or_else(|| env::var("PRIME_PLATFORM").ok())
            .or_else(|| manifest_build.and_then(|b| b.platform.clone()))
            .filter(|p| !p.trim().is_empty());
        let toolchain = ToolchainSettings {
            cc: env::var("PRIME_RISCV_CC")
                .ok()
                .or_else(|| manifest_build.and_then(|b| b.toolchain.cc.clone())),
            ar: env::var("PRIME_RISCV_AR")
                .ok()
                .or_else(|| manifest_build.and_then(|b| b.toolchain.ar.clone())),
            objcopy: env::var("PRIME_RISCV_OBJCOPY")
                .ok()
                .or_else(|| manifest_build.and_then(|b| b.toolchain.objcopy.clone())),
            ld_script: env::var("PRIME_RISCV_LD_SCRIPT")
                .ok()
                .or_else(|| manifest_build.and_then(|b| b.toolchain.ld_script.clone())),
            startup_obj: env::var("PRIME_RISCV_STARTUP_OBJ")
                .ok()
                .or_else(|| manifest_build.and_then(|b| b.toolchain.startup_obj.clone())),
            ld_flags: env::var("PRIME_RISCV_LD_FLAGS")
                .ok()
                .or_else(|| manifest_build.and_then(|b| b.toolchain.ld_flags.clone())),
            esptool: env::var("PRIME_ESPTOOL")
                .ok()
                .or_else(|| manifest_build.and_then(|b| b.toolchain.esptool.clone())),
            env: manifest_build.and_then(|b| b.toolchain.env.clone()),
        };
        let flash = manifest_build
            .map(|b| FlashSettings {
                enabled: b.flash.enabled,
                port: b.flash.port.clone(),
                baud: b.flash.baud,
                address: b.flash.address.clone(),
            })
            .unwrap_or_default();
        Self {
            target,
            platform,
            toolchain,
            flash,
        }
    }
}

impl Default for BuildOptions {
    fn default() -> Self {
        Self {
            target: BuildTarget::host(),
            platform: None,
            toolchain: ToolchainSettings::default(),
            flash: FlashSettings::default(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct FlashSettings {
    pub enabled: bool,
    pub port: Option<String>,
    pub baud: Option<u32>,
    pub address: Option<String>,
}
