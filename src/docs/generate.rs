use crate::language::{ast::Item, parser::parse_module};
use crate::project::{find_manifest, manifest::PackageManifest};
use crate::project::manifest::canonical_module_name;
use std::fmt::Write as _;
use std::fs;
use std::io::Write;
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::collections::HashSet;
use toml::Value;

fn escape_html(input: &str) -> String {
    input
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

fn render_item(item: &Item, out: &mut String) {
    match item {
        Item::Struct(def) => {
            let _ = writeln!(
                out,
                "<div class=\"item struct\"><h3>struct {}</h3>",
                def.name
            );
            if let Some(doc) = &def.doc {
                let _ = writeln!(out, "<pre>{}</pre>", escape_html(doc));
            }
            let _ = writeln!(out, "</div>");
        }
        Item::Enum(def) => {
            let _ = writeln!(out, "<div class=\"item enum\"><h3>enum {}</h3>", def.name);
            if let Some(doc) = &def.doc {
                let _ = writeln!(out, "<pre>{}</pre>", escape_html(doc));
            }
            let _ = writeln!(out, "</div>");
        }
        Item::Interface(def) => {
            let _ = writeln!(
                out,
                "<div class=\"item interface\"><h3>interface {}</h3>",
                def.name
            );
            if let Some(doc) = &def.doc {
                let _ = writeln!(out, "<pre>{}</pre>", escape_html(doc));
            }
            let _ = writeln!(out, "</div>");
        }
        Item::Impl(def) => {
            let _ = writeln!(
                out,
                "<div class=\"item impl\"><h3>impl {} for {}</h3>",
                def.interface, def.target
            );
            if let Some(doc) = &def.doc {
                let _ = writeln!(out, "<pre>{}</pre>", escape_html(doc));
            }
            let _ = writeln!(out, "</div>");
        }
        Item::Function(def) => {
            let _ = writeln!(out, "<div class=\"item fn\"><h3>fn {}</h3>", def.name);
            if let Some(doc) = &def.doc {
                let _ = writeln!(out, "<pre>{}</pre>", escape_html(doc));
            }
            let _ = writeln!(out, "</div>");
        }
        Item::Macro(def) => {
            let _ = writeln!(
                out,
                "<div class=\"item macro\"><h3>macro ~{}</h3>",
                def.name
            );
            if let Some(doc) = &def.doc {
                let _ = writeln!(out, "<pre>{}</pre>", escape_html(doc));
            }
            let _ = writeln!(out, "</div>");
        }
        Item::Const(def) => {
            let _ = writeln!(out, "<div class=\"item const\"><h3>const {}</h3>", def.name);
            if let Some(doc) = &def.doc {
                let _ = writeln!(out, "<pre>{}</pre>", escape_html(doc));
            }
            let _ = writeln!(out, "</div>");
        }
        Item::MacroInvocation(_) => {}
        Item::Comment { .. } => {}
    }
}

pub fn generate_docs(out: Option<PathBuf>) -> Result<(), String> {
    let html = build_docs_html()?;
    if let Some(path) = out {
        fs::write(&path, html).map_err(|err| format!("failed to write {}: {}", path.display(), err))
    } else {
        println!("{html}");
        Ok(())
    }
}

fn build_docs_html() -> Result<String, String> {
    let manifest_path = find_manifest(Path::new("."))
        .ok_or_else(|| "prime.toml not found; run from a project root".to_string())?;
    let mut manifests = Vec::new();

    // Load root manifest
    let root_manifest = PackageManifest::load(&manifest_path).map_err(|err| {
        format!(
            "failed to load manifest {}: {:?}",
            manifest_path.display(),
            err
        )
    })?;

    // Discover workspace members (if any)
    let mut workspace_members: Vec<PathBuf> = Vec::new();
    if let Ok(text) = fs::read_to_string(&manifest_path) {
        if let Ok(value) = text.parse::<Value>() {
            if let Some(ws) = value.get("workspace").and_then(|v| v.as_table()) {
                if let Some(members) = ws.get("members").and_then(|v| v.as_array()) {
                    let root_dir = manifest_path
                        .parent()
                        .map(|p| p.to_path_buf())
                        .unwrap_or_else(|| PathBuf::from("."));
                    for member in members {
                        if let Some(path_str) = member.as_str() {
                            let member_manifest = root_dir.join(path_str).join("prime.toml");
                            if member_manifest.exists() {
                                workspace_members.push(member_manifest);
                            }
                        }
                    }
                }
            }
        }
    }

    if workspace_members.is_empty() {
        manifests.push(root_manifest);
    } else {
        manifests.push(root_manifest);
        for path in workspace_members {
            match PackageManifest::load(&path) {
                Ok(m) => manifests.push(m),
                Err(err) => {
                    eprintln!(
                        "warning: failed to load workspace member manifest {}: {:?}",
                        path.display(),
                        err
                    );
                }
            }
        }
    }
    let mut modules = Vec::new();
    let mut seen_paths: HashSet<PathBuf> = HashSet::new();

    for manifest in &manifests {
        let entries = manifest.module_entries();
        let mut had_entries = false;
        for entry in entries {
            had_entries = true;
            let path = if entry.path.is_absolute() {
                entry.path.clone()
            } else {
                manifest.root().join(&entry.path)
            };
            let canonical = path
                .canonicalize()
                .unwrap_or_else(|_| path.clone());
            if !seen_paths.insert(canonical.clone()) {
                continue;
            }
            let source = fs::read_to_string(&path)
                .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
            let parsed = parse_module(&entry.name, path.clone(), &source).map_err(|err| {
                let msgs: Vec<String> = err
                    .errors
                    .iter()
                    .map(|e| format!("{}: {}", path.display(), e.message))
                    .collect();
                msgs.join("\n")
            })?;
            modules.push(parsed);
        }

        if !had_entries {
            let mut candidates = Vec::new();
            collect_prime_files(manifest.root(), &mut candidates);
            for path in candidates {
                let canonical = path
                    .canonicalize()
                    .unwrap_or_else(|_| path.clone());
                if !seen_paths.insert(canonical.clone()) {
                    continue;
                }
                let name = manifest
                    .module_name_for_path(&canonical)
                    .unwrap_or_else(|| {
                        let stem = path
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .unwrap_or("module");
                        canonical_module_name(stem)
                    });
                let source = fs::read_to_string(&path)
                    .map_err(|err| format!("failed to read {}: {}", path.display(), err))?;
                let parsed = parse_module(&name, path.clone(), &source).map_err(|err| {
                    let msgs: Vec<String> = err
                        .errors
                        .iter()
                        .map(|e| format!("{}: {}", path.display(), e.message))
                        .collect();
                    msgs.join("\n")
                })?;
                modules.push(parsed);
            }
        }
    }

    let mut html = String::new();
    html.push_str("<html><head><title>Prime Docs</title>");
    html.push_str("<style>body{font-family:monospace;padding:1rem;}h2{margin-top:2rem;}pre{background:#111;color:#f2f2f2;padding:0.5rem;} .item{margin-bottom:1rem;} .item h3{margin-bottom:0.2rem;}</style>");
    html.push_str("</head><body>");
    html.push_str("<h1>Prime Documentation</h1>");

    for module in &modules {
        let _ = writeln!(html, "<h2>module {}</h2>", module.name);
        if let Some(doc) = &module.doc {
            let _ = writeln!(html, "<pre>{}</pre>", escape_html(doc));
        }
        for item in &module.items {
            render_item(item, &mut html);
        }
    }

    html.push_str("</body></html>");

    Ok(html)
}

pub fn serve_docs(port: u16) -> Result<(), String> {
    let html = build_docs_html()?;
    let listener = TcpListener::bind(("127.0.0.1", port))
        .map_err(|err| format!("failed to bind 127.0.0.1:{port}: {err}"))?;
    println!("Serving docs at http://127.0.0.1:{port} (Ctrl+C to stop)");
    for stream in listener.incoming() {
        let mut stream = stream.map_err(|err| format!("incoming connection failed: {err}"))?;
        let response = format!(
            "HTTP/1.1 200 OK\r\ncontent-type: text/html; charset=utf-8\r\ncontent-length: {}\r\n\r\n{}",
            html.len(),
            html
        );
        stream
            .write_all(response.as_bytes())
            .map_err(|err| format!("failed to write response: {err}"))?;
    }
    Ok(())
}

fn collect_prime_files(root: &Path, out: &mut Vec<PathBuf>) {
    if let Ok(entries) = fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if name == "target" || name == ".prime" || name.starts_with('.') {
                        continue;
                    }
                }
                collect_prime_files(&path, out);
            } else if path.extension().and_then(|e| e.to_str()) == Some("prime") {
                out.push(path);
            }
        }
    }
}
