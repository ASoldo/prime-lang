use crate::{
    language::{
        errors::SyntaxError,
        macro_expander::expand_program,
        span::Span,
        typecheck::{TypecheckOptions, check_program_with_options},
    },
    project::{
        EntryPoint, PackageError, load_manifest_for_entry, load_resolved_package,
        lock::require_frozen, resolve_entry_path,
    },
    target::BuildOptions,
};
use clap::ValueEnum;
use serde::Serialize;
use std::{
    io::{self, Write},
    path::Path,
    process::ExitCode,
};

#[derive(Clone, Copy, Debug, Default, ValueEnum)]
pub enum CheckFormat {
    #[default]
    Text,
    Json,
}

#[derive(Serialize)]
struct Report<'a> {
    schema_version: u32,
    outcome: &'static str,
    target: &'a str,
    entry_path: Option<String>,
    manifest_path: Option<String>,
    paths: Vec<String>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord)]
struct ByteSpan {
    start: usize,
    end: usize,
}

impl From<Span> for ByteSpan {
    fn from(span: Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

// Field order also defines diagnostic ordering in both output formats.
#[derive(Serialize, PartialEq, Eq, PartialOrd, Ord)]
struct Diagnostic {
    path: String,
    span: Option<ByteSpan>,
    phase: &'static str,
    message: String,
    code: Option<String>,
    help: Option<String>,
}

impl Diagnostic {
    fn new(path: &Path, phase: &'static str, message: String) -> Self {
        Self {
            path: path.display().to_string(),
            span: None,
            phase,
            message,
            code: None,
            help: None,
        }
    }
}

fn source_errors(
    path: &Path,
    phase: &'static str,
    errors: Vec<SyntaxError>,
) -> impl Iterator<Item = Diagnostic> {
    errors.into_iter().map(move |err| Diagnostic {
        span: Some(err.span.into()),
        help: err.help,
        ..Diagnostic::new(path, phase, err.message)
    })
}

fn package_errors(err: PackageError) -> Vec<Diagnostic> {
    match err {
        PackageError::Io { path, error } => {
            vec![Diagnostic::new(&path, "io", error.to_string())]
        }
        PackageError::Manifest { path, message } => {
            vec![Diagnostic::new(&path, "manifest", message)]
        }
        PackageError::Syntax(files) => {
            let mut diagnostics = Vec::new();
            for file in files {
                diagnostics.extend(source_errors(&file.path, "syntax", file.errors));
            }
            diagnostics
        }
    }
}

impl Report<'_> {
    fn check(&mut self, project: Option<&str>, frozen: bool) -> Result<(), Vec<Diagnostic>> {
        let entry = EntryPoint::from_target(self.target);
        let manifest = load_manifest_for_entry(entry, project).map_err(package_errors)?;
        self.manifest_path = manifest.as_ref().map(|m| m.path.display().to_string());
        if project.is_some() && manifest.is_none() {
            return Err(vec![Diagnostic::new(
                Path::new("prime.toml"),
                "manifest",
                "--project requires a workspace manifest".into(),
            )]);
        }
        let path = resolve_entry_path(entry, manifest.as_ref(), project).map_err(package_errors)?;
        if path.extension().is_none_or(|ext| ext != "prime") {
            return Err(vec![Diagnostic::new(
                &path,
                "input",
                "expected a .prime source file".into(),
            )]);
        }
        let path = path
            .canonicalize()
            .map_err(|err| vec![Diagnostic::new(&path, "io", err.to_string())])?;
        self.entry_path = Some(path.display().to_string());
        if frozen {
            require_frozen(manifest.as_ref(), &path).map_err(package_errors)?;
        }

        let options = TypecheckOptions {
            target: BuildOptions::from_sources(None, None, manifest.as_ref()).target,
        };
        let package = load_resolved_package(&path, manifest).map_err(package_errors)?;
        self.paths = package
            .program
            .modules
            .iter()
            .map(|module| module.path.display().to_string())
            .collect();
        self.paths.sort_unstable();
        self.paths.dedup();

        let expanded = expand_program(&package.program).map_err(|files| {
            let mut diagnostics = Vec::new();
            for file in files {
                diagnostics.extend(source_errors(&file.path, "macro", file.errors));
            }
            diagnostics
        })?;
        check_program_with_options(&expanded, &options).map_err(|errors| {
            errors
                .into_iter()
                .map(|err| Diagnostic {
                    span: Some(err.span.into()),
                    code: err.code,
                    help: err.help,
                    ..Diagnostic::new(&err.path, "type", err.message)
                })
                .collect()
        })
    }

    fn print(&self, format: CheckFormat) -> io::Result<()> {
        match format {
            CheckFormat::Json => {
                let mut stdout = io::stdout().lock();
                serde_json::to_writer(&mut stdout, self)?;
                writeln!(stdout)
            }
            CheckFormat::Text if self.outcome == "ok" => {
                let count = self.paths.len();
                let unit = if count == 1 {
                    "source file"
                } else {
                    "source files"
                };
                writeln!(
                    io::stdout().lock(),
                    "Checked {} ({count} {unit})",
                    self.target
                )
            }
            CheckFormat::Text => {
                let mut stderr = io::stderr().lock();
                for err in &self.diagnostics {
                    write!(stderr, "{}", err.path)?;
                    if let Some(span) = &err.span {
                        write!(stderr, ":{}..{}", span.start, span.end)?;
                    }
                    write!(stderr, ": {} error", err.phase)?;
                    if let Some(code) = &err.code {
                        write!(stderr, " [{code}]")?;
                    }
                    writeln!(stderr, ": {}", err.message)?;
                    if let Some(help) = &err.help {
                        writeln!(stderr, "  help: {help}")?;
                    }
                }
                Ok(())
            }
        }
    }
}

/// Resolve, load, expand, and typecheck a target without running Prime code.
pub fn run_check(
    entry: &str,
    project: Option<&str>,
    frozen: bool,
    format: CheckFormat,
) -> ExitCode {
    let mut report = Report {
        schema_version: 1,
        outcome: "ok",
        target: entry,
        entry_path: None,
        manifest_path: None,
        paths: Vec::new(),
        diagnostics: Vec::new(),
    };
    let mut status = ExitCode::SUCCESS;
    if let Err(mut diagnostics) = report.check(project, frozen) {
        diagnostics.sort_unstable();
        report.diagnostics = diagnostics;
        report.outcome = "error";
        status = ExitCode::FAILURE;
    }
    if let Err(err) = report.print(format) {
        eprintln!("could not write the check report: {err}");
        return ExitCode::FAILURE;
    }
    status
}
