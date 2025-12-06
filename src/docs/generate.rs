use crate::language::{ast::Item, parser::parse_module};
use crate::project::manifest::canonical_module_name;
use crate::project::{find_manifest, manifest::PackageManifest};
use serde::Serialize;
use std::collections::HashSet;
use std::fs;
use std::io::Write;
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use toml::Value;

#[derive(Debug, Clone, Serialize)]
struct DocModule {
    name: String,
    doc: Option<String>,
    items: Vec<DocItem>,
}

#[derive(Debug, Clone, Serialize)]
struct DocItem {
    name: String,
    kind: String,
    signature: String,
    doc: Option<String>,
}

fn item_to_doc(item: &Item) -> Option<DocItem> {
    match item {
        Item::Struct(def) => Some(DocItem {
            name: def.name.clone(),
            kind: "struct".into(),
            signature: format!("struct {}", def.name),
            doc: def.doc.clone(),
        }),
        Item::Enum(def) => Some(DocItem {
            name: def.name.clone(),
            kind: "enum".into(),
            signature: format!("enum {}", def.name),
            doc: def.doc.clone(),
        }),
        Item::Interface(def) => Some(DocItem {
            name: def.name.clone(),
            kind: "interface".into(),
            signature: format!("interface {}", def.name),
            doc: def.doc.clone(),
        }),
        Item::Impl(def) => {
            let sig = if def.inherent {
                format!("impl {}", def.target)
            } else {
                format!("impl {} for {}", def.interface, def.target)
            };
            Some(DocItem {
                name: sig.clone(),
                kind: "impl".into(),
                signature: sig,
                doc: def.doc.clone(),
            })
        }
        Item::Function(def) => Some(DocItem {
            name: def.name.clone(),
            kind: "fn".into(),
            signature: format!("fn {}", def.name),
            doc: def.doc.clone(),
        }),
        Item::Macro(def) => Some(DocItem {
            name: def.name.clone(),
            kind: "macro".into(),
            signature: format!("macro ~{}", def.name),
            doc: def.doc.clone(),
        }),
        Item::Const(def) => Some(DocItem {
            name: def.name.clone(),
            kind: "const".into(),
            signature: format!("const {}", def.name),
            doc: def.doc.clone(),
        }),
        Item::MacroInvocation(_) | Item::Comment { .. } => None,
    }
}

fn collect_modules(manifests: &[PackageManifest]) -> Result<Vec<DocModule>, String> {
    let mut modules = Vec::new();
    let mut seen_paths: HashSet<PathBuf> = HashSet::new();

    for manifest in manifests {
        let entries = manifest.module_entries();
        let mut had_entries = false;
        for entry in entries {
            had_entries = true;
            let path = if entry.path.is_absolute() {
                entry.path.clone()
            } else {
                manifest.root().join(&entry.path)
            };
            let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
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
            modules.push(DocModule {
                name: parsed.name.clone(),
                doc: parsed.doc.clone(),
                items: parsed.items.iter().filter_map(item_to_doc).collect(),
            });
        }

        if !had_entries {
            let mut candidates = Vec::new();
            collect_prime_files(manifest.root(), &mut candidates);
            for path in candidates {
                let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
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
                modules.push(DocModule {
                    name: parsed.name.clone(),
                    doc: parsed.doc.clone(),
                    items: parsed.items.iter().filter_map(item_to_doc).collect(),
                });
            }
        }
    }

    Ok(modules)
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
    let modules = collect_modules(&manifests)?;
    let data = serde_json::to_string(&modules).map_err(|e| e.to_string())?;

    let mut html = String::new();
    html.push_str("<!doctype html><html><head><meta charset=\"utf-8\"><title>Prime Docs</title>");
    html.push_str(
        r#"<style>
:root {
  --bg: #0f1117;
  --panel: #161925;
  --panel-soft: #1e2230;
  --text: #e8ecf2;
  --muted: #8c93a5;
  --accent: #63a4ff;
  --border: #252a38;
  --code: #0c0f16;
  --shadow: 0 8px 20px rgba(0,0,0,0.35);
}
* { box-sizing: border-box; }
body {
  margin: 0;
  background: radial-gradient(circle at 20% 20%, #121627, #0c0f16 45%), radial-gradient(circle at 80% 10%, #101425, #0c0f16 40%);
  color: var(--text);
  font-family: "JetBrains Mono", "SFMono-Regular", Menlo, Consolas, monospace;
}
*::-webkit-scrollbar {
  width: 10px;
}
*::-webkit-scrollbar-track {
  background: var(--panel);
}
*::-webkit-scrollbar-thumb {
  background: var(--border);
  border-radius: 6px;
}
*::-webkit-scrollbar-thumb:hover {
  background: var(--muted);
}
.app {
  display: grid;
  grid-template-rows: auto 1fr;
  height: 100vh;
}
.topbar {
  display: flex;
  gap: 12px;
  align-items: center;
  padding: 12px 16px;
  background: rgba(18, 22, 39, 0.92);
  border-bottom: 1px solid var(--border);
  box-shadow: var(--shadow);
  position: sticky;
  top: 0;
  z-index: 10;
}
.search {
  flex: 1;
  position: relative;
}
.search input {
  width: 100%;
  padding: 10px 12px;
  border-radius: 8px;
  border: 1px solid var(--border);
  background: var(--panel-soft);
  color: var(--text);
  font-size: 14px;
}
.search button.clear {
  position: absolute;
  right: 8px;
  top: 8px;
  width: 26px;
  height: 26px;
  border-radius: 6px;
  border: 1px solid var(--border);
  background: var(--panel);
  color: var(--muted);
  cursor: pointer;
  display: none;
}
.search button.clear.visible { display: inline-flex; align-items: center; justify-content: center; }
.search button.clear:hover { color: var(--text); border-color: var(--muted); }
.main {
  display: grid;
  grid-template-columns: 280px 1fr 220px;
  gap: 12px;
  padding: 12px 12px 16px;
  height: calc(100vh - 64px);
}
.panel {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 12px;
  overflow: hidden;
  box-shadow: var(--shadow);
}
.scroll {
  overflow: auto;
  height: 100%;
}
.tree h3, .outline h3 { margin: 0 0 8px 0; color: var(--muted); font-size: 13px; letter-spacing: .03em; text-transform: uppercase;}
.module { margin-bottom: 10px; }
.module button {
  width: 100%;
  background: none;
  border: none;
  color: var(--text);
  text-align: left;
  padding: 6px 6px;
  border-radius: 6px;
  cursor: pointer;
}
.module button.active { background: var(--panel-soft); color: var(--accent); }
.item-list { margin-left: 8px; }
.item { padding: 4px 6px; border-radius: 6px; cursor: pointer; color: var(--muted); }
.item:hover { background: var(--panel-soft); color: var(--text); }
.content { padding: 8px; }
.content h2 { margin-top: 0; }
.card {
  background: var(--panel-soft);
  border: 1px solid var(--border);
  border-radius: 10px;
  padding: 10px 12px;
  margin-bottom: 12px;
}
.chip { display: inline-block; padding: 2px 8px; background: var(--code); color: var(--accent); border-radius: 999px; font-size: 12px; margin-right: 6px; }
.sig { color: var(--text); font-weight: 600; }
.doc { color: var(--muted); white-space: pre-wrap; margin-top: 6px; }
.outline a { display: block; color: var(--muted); padding: 4px 6px; border-radius: 6px; text-decoration: none; }
.outline a:hover { background: var(--panel-soft); color: var(--text); }
    </style>"#,
    );
    html.push_str("</head><body><div class=\"app\">");
    html.push_str(
        r#"<div class="topbar">
  <div class="logo" style="font-weight:700;color:var(--accent);">Prime Docs</div>
  <div class="search">
    <input id="search" type="text" placeholder="Search modules, symbols...">
    <button id="search-clear" class="clear" aria-label="Clear search">✕</button>
  </div>
</div>
<div class="main">
  <div class="panel tree"><div class="scroll"><h3>Workspace</h3><div id="nav"></div></div></div>
  <div class="panel"><div class="scroll content" id="content"></div></div>
  <div class="panel outline"><div class="scroll"><h3>Outline</h3><div id="outline"></div></div></div>
</div>
</div>
<script>
const data = __DATA__;
const modules = data;
const navEl = document.getElementById('nav');
const contentEl = document.getElementById('content');
const outlineEl = document.getElementById('outline');
const searchEl = document.getElementById('search');
const searchClear = document.getElementById('search-clear');
let activeModule = 0;
let filterTerm = '';
let expanded = new Set();

function renderNav(filter='') {
  filterTerm = filter;
  navEl.innerHTML = '';
  modules.forEach((m, idx) => {
    const matches = !filter || m.name.toLowerCase().includes(filter) || m.items.some(it => it.name.toLowerCase().includes(filter) || it.signature.toLowerCase().includes(filter));
    if (!matches) return;
    const wrap = document.createElement('div');
    wrap.className = 'module';
    const btn = document.createElement('button');
    btn.textContent = m.name;
    if (idx === activeModule) btn.classList.add('active');
    const isOpen = expanded.has(idx) || filter;
    btn.onclick = () => {
      if (expanded.has(idx)) {
        expanded.delete(idx);
      } else {
        expanded.add(idx);
      }
      activeModule = idx;
      renderAll(filterTerm);
    };
    wrap.appendChild(btn);
    const list = document.createElement('div');
    list.className = 'item-list';
    if (isOpen) {
      m.items.forEach((it, iidx) => {
        if (filter && !(it.name.toLowerCase().includes(filter) || it.kind.toLowerCase().includes(filter) || it.signature.toLowerCase().includes(filter))) return;
        const item = document.createElement('div');
        item.className = 'item';
        item.textContent = `${it.kind} ${it.name}`;
        item.onclick = () => {
          activeModule = idx;
          renderContent();
          const anchor = document.getElementById(anchorId(idx, iidx));
          if (anchor) anchor.scrollIntoView({behavior:'smooth', block:'start'});
        };
        list.appendChild(item);
      });
      wrap.appendChild(list);
    }
    navEl.appendChild(wrap);
  });
}
function anchorId(mid, iid){ return `m${mid}-i${iid}`; }
function renderContent() {
  const m = modules[activeModule] || modules[0];
  contentEl.innerHTML = '';
  const title = document.createElement('h2');
  title.textContent = m.name;
  contentEl.appendChild(title);
  if (m.doc) {
    const doc = document.createElement('div');
    doc.className = 'doc';
    doc.textContent = m.doc;
    contentEl.appendChild(doc);
  }
  m.items.forEach((it, idx) => {
    const card = document.createElement('div');
    card.className = 'card';
    card.id = anchorId(activeModule, idx);
    const top = document.createElement('div');
    const chip = document.createElement('span');
    chip.className = 'chip';
    chip.textContent = it.kind;
    top.appendChild(chip);
    const sig = document.createElement('span');
    sig.className = 'sig';
    sig.textContent = it.signature;
    top.appendChild(sig);
    card.appendChild(top);
    if (it.doc) {
      const doc = document.createElement('div');
      doc.className = 'doc';
      doc.textContent = it.doc;
      card.appendChild(doc);
    }
    contentEl.appendChild(card);
  });
  renderOutline();
}
function renderOutline() {
  outlineEl.innerHTML = '';
  const m = modules[activeModule] || modules[0];
  m.items.forEach((it, idx) => {
    const a = document.createElement('a');
    a.href = `#${anchorId(activeModule, idx)}`;
    a.textContent = it.signature;
    outlineEl.appendChild(a);
  });
}
function renderAll(filter='') {
  renderNav(filter);
  renderContent();
}
searchEl.addEventListener('input', (e)=> {
  renderNav(e.target.value.toLowerCase());
});
searchEl.addEventListener('keypress', (e) => {
  if (e.key === 'Enter') {
    renderAll(e.target.value.toLowerCase());
  }
});
searchClear.addEventListener('click', ()=> {
  searchEl.value = '';
  filterTerm = '';
  renderAll('');
});
searchEl.addEventListener('input', () => {
  if (searchEl.value) {
    searchClear.classList.add('visible');
  } else {
    searchClear.classList.remove('visible');
  }
});
renderAll();
</script>
"#,
    );
    let safe_data = data.replace("</script>", "<\\/script>");
    let final_html = html.replace("__DATA__", &safe_data);
    Ok(final_html)
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
