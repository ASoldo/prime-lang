use super::manifest::{ModuleVisibility, PackageManifest};
use crate::language::{
    ast::{ImportPath, Module},
    span::Span,
};
use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

pub const CODE_MISSING_MODULE_HEADER: &str = "prime.missingModuleHeader";
pub const CODE_MANIFEST_MISSING_MODULE: &str = "prime.manifestMissingModule";
pub const CODE_DUPLICATE_IMPORT: &str = "prime.duplicateImport";
pub const CODE_UNKNOWN_IMPORT: &str = "prime.unknownImport";

#[derive(Debug, Clone)]
pub struct ManifestIssue {
    pub span: Option<Span>,
    pub kind: ManifestIssueKind,
}

#[derive(Debug, Clone)]
pub enum ManifestIssueKind {
    ModuleNameMismatch {
        expected: String,
        actual: String,
    },
    MissingModuleHeader {
        expected: String,
    },
    DeclaredModuleNotInManifest {
        declared: String,
        manifest_path: PathBuf,
        module_path: String,
    },
    DuplicateModuleDeclaration,
    DuplicateImport {
        module: String,
    },
    ManifestMissingModule {
        module: String,
        module_path: String,
        manifest_path: PathBuf,
        visibility: ModuleVisibility,
    },
    UnknownImport {
        module: String,
    },
}

impl ManifestIssueKind {
    pub fn message(&self) -> String {
        match self {
            ManifestIssueKind::ModuleNameMismatch { expected, actual } => {
                format!("Module declared as `{actual}` but manifest maps this file to `{expected}`")
            }
            ManifestIssueKind::MissingModuleHeader { expected } => format!(
                "Manifest maps this file to `{expected}` but the file is missing `module {expected};`"
            ),
            ManifestIssueKind::DeclaredModuleNotInManifest {
                declared,
                manifest_path,
                ..
            } => format!(
                "Module `{declared}` is declared here but not listed in prime.toml ({})",
                manifest_path.display()
            ),
            ManifestIssueKind::DuplicateModuleDeclaration => {
                "Duplicate `module` declaration; only the first declaration is used".into()
            }
            ManifestIssueKind::DuplicateImport { module } => {
                format!("Duplicate import `{module}`")
            }
            ManifestIssueKind::ManifestMissingModule {
                module,
                module_path,
                manifest_path,
                ..
            } => format!(
                "Module `{module}` exists at `{module_path}` but is not listed in prime.toml ({})",
                manifest_path.display()
            ),
            ManifestIssueKind::UnknownImport { module } => {
                format!("Cannot resolve import `{module}` — no manifest entry or file found")
            }
        }
    }
}

pub fn analyze_manifest_issues(
    module: &Module,
    file_path: &Path,
    manifest: Option<&PackageManifest>,
) -> Vec<ManifestIssue> {
    let mut issues = Vec::new();
    if let Some(manifest) = manifest {
        issues.extend(module_declaration_issues(module, file_path, manifest));
        issues.extend(import_issues(module, file_path, manifest));
    }
    issues
}

fn module_declaration_issues(
    module: &Module,
    file_path: &Path,
    manifest: &PackageManifest,
) -> Vec<ManifestIssue> {
    let mut issues = Vec::new();
    let expected = manifest.module_name_for_path(file_path);
    let declared = module.declared_name.as_deref();
    match (expected.as_deref(), declared) {
        (Some(expected), Some(actual)) if expected != actual => issues.push(ManifestIssue {
            span: module.declared_span,
            kind: ManifestIssueKind::ModuleNameMismatch {
                expected: expected.to_string(),
                actual: actual.to_string(),
            },
        }),
        (Some(expected), None) => issues.push(ManifestIssue {
            span: module.declared_span.or_else(|| Some(Span::new(0, 0))),
            kind: ManifestIssueKind::MissingModuleHeader {
                expected: expected.to_string(),
            },
        }),
        (None, Some(actual)) => {
            let module_path = manifest_relative_string(file_path, manifest);
            issues.push(ManifestIssue {
                span: module.declared_span,
                kind: ManifestIssueKind::DeclaredModuleNotInManifest {
                    declared: actual.to_string(),
                    manifest_path: manifest.path.clone(),
                    module_path,
                },
            });
        }
        _ => {}
    }
    for span in &module.redundant_module_spans {
        issues.push(ManifestIssue {
            span: Some(*span),
            kind: ManifestIssueKind::DuplicateModuleDeclaration,
        });
    }
    issues
}

fn import_issues(
    module: &Module,
    file_path: &Path,
    manifest: &PackageManifest,
) -> Vec<ManifestIssue> {
    let mut issues = Vec::new();
    let mut seen = HashSet::new();
    for import in &module.imports {
        let name = import.path.to_string();
        if !seen.insert(name.clone()) {
            issues.push(ManifestIssue {
                span: Some(import.span),
                kind: ManifestIssueKind::DuplicateImport { module: name },
            });
            continue;
        }
        if manifest.module_path(&name).is_some() {
            continue;
        }
        let resolved = resolve_import_path(file_path, &import.path);
        if resolved.exists() {
            let module_path = manifest_relative_string(&resolved, manifest);
            issues.push(ManifestIssue {
                span: Some(import.span),
                kind: ManifestIssueKind::ManifestMissingModule {
                    module: name,
                    module_path,
                    manifest_path: manifest.path.clone(),
                    visibility: ModuleVisibility::Public,
                },
            });
        } else {
            issues.push(ManifestIssue {
                span: Some(import.span),
                kind: ManifestIssueKind::UnknownImport { module: name },
            });
        }
    }
    issues
}

pub fn manifest_relative_string(path: &Path, manifest: &PackageManifest) -> String {
    let relative = path.strip_prefix(manifest.root()).unwrap_or(path);
    relative
        .components()
        .map(|component| component.as_os_str().to_string_lossy())
        .collect::<Vec<_>>()
        .join("/")
}

pub fn resolve_import_path(base: &Path, import_path: &ImportPath) -> PathBuf {
    let mut path = import_path.to_relative_path();
    if path.extension().and_then(|ext| ext.to_str()) != Some("prime") {
        path.set_extension("prime");
    }
    if path.is_absolute() {
        return path;
    }
    let base_dir = base
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));
    base_dir.join(path)
}
