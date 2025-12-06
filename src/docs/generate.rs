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
    imports: Vec<String>,
    edges: Vec<DocEdge>,
}

#[derive(Debug, Clone, Serialize)]
struct DocItem {
    name: String,
    kind: String,
    signature: String,
    doc: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
struct DocEdge {
    from: String,
    to: String,
    kind: String,
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

fn build_edges(items: &[Item], docs: &[DocItem]) -> Vec<DocEdge> {
    let mut edges = Vec::new();
    let mut name_to_kind = std::collections::HashMap::new();
    for doc in docs {
        name_to_kind.insert(doc.name.clone(), doc.kind.clone());
    }

    for item in items {
        if let Item::Impl(def) = item {
            let sig = if def.inherent {
                format!("impl {}", def.target)
            } else {
                format!("impl {} for {}", def.interface, def.target)
            };
            let from = format!("impl:{}", sig);
            let target = def.target.clone();
            if name_to_kind.contains_key(&target) {
                edges.push(DocEdge {
                    from: from.clone(),
                    to: format!("{}:{}", name_to_kind[&target], target),
                    kind: "impl-target".into(),
                });
            }
            if !def.interface.is_empty() && name_to_kind.contains_key(&def.interface) {
                edges.push(DocEdge {
                    from,
                    to: format!("{}:{}", name_to_kind[&def.interface], def.interface),
                    kind: "impl-interface".into(),
                });
            }
        }
    }
    edges
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
            let imports = parsed
                .imports
                .iter()
                .map(|imp| imp.path.to_string())
                .collect();
            let items: Vec<DocItem> = parsed.items.iter().filter_map(item_to_doc).collect();
            let edges = build_edges(&parsed.items, &items);
            modules.push(DocModule {
                name: parsed.name.clone(),
                doc: parsed.doc.clone(),
                items,
                imports,
                edges,
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
                let imports = parsed
                    .imports
                    .iter()
                    .map(|imp| imp.path.to_string())
                    .collect();
                let items: Vec<DocItem> = parsed.items.iter().filter_map(item_to_doc).collect();
                let edges = build_edges(&parsed.items, &items);
                modules.push(DocModule {
                    name: parsed.name.clone(),
                    doc: parsed.doc.clone(),
                    items,
                    imports,
                    edges,
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
  --fn: #7dd3fc;
  --macro: #f472b6;
  --const: #c084fc;
  --struct: #34d399;
  --enum: #fb923c;
  --interface: #facc15;
  --impl: #a5b4fc;
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
  border-radius: 999px;
  border: 1px solid var(--border);
  background: #f5f5f5;
  color: #000 !important;
  font-weight: 700;
  cursor: pointer;
  display: none;
}
.search button.clear.visible { display: inline-flex; align-items: center; justify-content: center; }
.search button.clear:hover { color: #000; border-color: #aaa; }
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
  display: flex;
  align-items: center;
  gap: 8px;
}
.module button.active { background: var(--panel-soft); color: var(--accent); }
.module .toggle {
  width: 16px;
  text-align: center;
  color: var(--muted);
}
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
.chip.fn { color: var(--fn); }
.chip.macro { color: var(--macro); }
.chip.const { color: var(--const); }
.chip.struct { color: var(--struct); }
.chip.enum { color: var(--enum); }
.chip.interface { color: var(--interface); }
.chip.impl { color: var(--impl); }
.sig { color: var(--text); font-weight: 600; }
.doc { color: var(--muted); white-space: pre-wrap; margin-top: 6px; }
.outline a { display: block; color: var(--muted); padding: 4px 6px; border-radius: 6px; text-decoration: none; transition: background 0.15s; }
.outline a:hover { background: var(--panel-soft); color: var(--text); }
.graph canvas { width: 100%; border: 1px solid var(--border); border-radius: 8px; background: var(--panel); }
/* Dark scrollbars */
.scroll::-webkit-scrollbar { width: 10px; height: 10px; }
.scroll::-webkit-scrollbar-track { background: #0f172a; border-radius: 10px; }
.scroll::-webkit-scrollbar-thumb { background: #3b4258; border-radius: 10px; border: 2px solid #0f172a; }
.scroll::-webkit-scrollbar-corner { background: #0f172a; }

.syntax-const { color: var(--const); }
.syntax-struct { color: var(--struct); }
.syntax-enum { color: var(--enum); }
.syntax-interface { color: var(--interface); }
.syntax-impl { color: var(--impl); }
.syntax-fn { color: var(--fn); }
.syntax-macro { color: var(--macro); }
.syntax-import { color: var(--muted); }
.syntax-ident { color: #7dd3fc; }
.syntax-key { color: #b983ff; }
    </style>"#,
    );
    html.push_str("</head><body><div class=\"app\">");
    html.push_str(
        r##"<div class="topbar">
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
let suppressPush = false;

function pushState(moduleIdx, anchor) {
  if (suppressPush) return;
  const hash = `#${modules[moduleIdx].name}${anchor ? `:${anchor}` : ''}`;
  history.pushState({ module: moduleIdx, anchor }, "", hash);
}

function renderNav(filter='') {
  filterTerm = filter;
  navEl.innerHTML = '';
  modules.forEach((m, idx) => {
    const matches = !filter || m.name.toLowerCase().includes(filter) || m.items.some(it => it.name.toLowerCase().includes(filter) || it.signature.toLowerCase().includes(filter));
    if (!matches) return;
    const wrap = document.createElement('div');
    wrap.className = 'module';
    const btn = document.createElement('button');
    const isOpen = expanded.has(idx) || filter;
    const toggle = document.createElement('span');
    toggle.className = 'toggle';
    toggle.textContent = isOpen ? '−' : '+';
    const label = document.createElement('span');
    label.innerHTML = formatName(m.name);
    btn.appendChild(toggle);
    btn.appendChild(label);
    if (idx === activeModule) btn.classList.add('active');
    btn.onclick = () => {
      if (expanded.has(idx)) {
        expanded.delete(idx);
      } else {
        expanded.add(idx);
      }
      selectModule(idx);
    };
    wrap.appendChild(btn);
    const list = document.createElement('div');
    list.className = 'item-list';
    if (isOpen) {
      m.items.forEach((it, iidx) => {
        if (filter && !(it.name.toLowerCase().includes(filter) || it.kind.toLowerCase().includes(filter) || it.signature.toLowerCase().includes(filter))) return;
        const item = document.createElement('div');
        item.className = `item syntax-${it.kind}`;
        item.innerHTML = renderSig(it.kind, it.name);
        item.onclick = () => {
          selectModule(idx, anchorId(idx, iidx));
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
  const graphCard = document.createElement('div');
  graphCard.className = 'card graph';
  const canvas = document.createElement('canvas');
  canvas.id = 'graph-canvas';
  const width = Math.max(800, contentEl.clientWidth ? contentEl.clientWidth - 40 : 900);
  canvas.width = width;
  canvas.height = 240;
  canvas.dataset.title = m.name;
  graphCard.appendChild(canvas);
  contentEl.appendChild(graphCard);
  const ctx = canvas.getContext('2d');
  let layout = computeLayout(canvas, m, ctx);
  drawGraph(ctx, layout);
  wireHover(canvas, layout);
  wireClicks(canvas, layout);
  if (m.doc) {
    const doc = document.createElement('div');
    doc.className = 'doc';
    doc.textContent = m.doc;
    contentEl.appendChild(doc);
  }
  m.items.forEach((it, idx) => {
    const card = document.createElement('div');
    card.className = 'card';
    card.dataset.key = `${it.kind}:${it.name}`;
    card.id = anchorId(activeModule, idx);
    const top = document.createElement('div');
    const chip = document.createElement('span');
    chip.className = `chip ${it.kind}`;
    chip.textContent = it.kind;
    top.appendChild(chip);
    const sig = document.createElement('span');
    sig.className = 'sig';
    sig.innerHTML = `<span class="syntax-key">${it.kind}</span> ${formatName(it.name)}`;
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
function selectModule(idx, anchor) {
  activeModule = idx;
  renderAll(filterTerm);
  const targetAnchor = anchor ? document.getElementById(anchor) : null;
  if (targetAnchor) {
    targetAnchor.scrollIntoView({behavior:'smooth', block:'start'});
  } else {
    contentEl.scrollTop = 0;
  }
  pushState(idx, anchor || null);
}
function renderOutline() {
  outlineEl.innerHTML = '';
  const m = modules[activeModule] || modules[0];
  (m.imports || []).forEach((name, idx) => {
    const a = document.createElement('a');
    a.href = "#";
    a.innerHTML = `<span class="syntax-key">import</span> ${formatName(name)}`;
    a.className = "outline-link syntax-import";
    a.dataset.key = `import:${name}`;
    a.onclick = (e) => {
      e.preventDefault();
      const targetIdx = modules.findIndex(mod => mod.name === name);
      if (targetIdx >= 0) {
        selectModule(targetIdx);
      }
    };
    outlineEl.appendChild(a);
  });
  m.items.forEach((it, idx) => {
    const a = document.createElement('a');
    a.href = `#${anchorId(activeModule, idx)}`;
    a.innerHTML = renderSig(it.kind, it.name);
    a.className = `outline-link syntax-${it.kind}`;
    a.dataset.key = `${it.kind}:${it.name}`;
    a.onclick = (e) => {
      e.preventDefault();
      selectModule(activeModule, anchorId(activeModule, idx));
    };
    outlineEl.appendChild(a);
  });
}

const colors = {
  import: "#94a3b8",
  fn: "#7dd3fc",
  macro: "#f472b6",
  const: "#c084fc",
  struct: "#34d399",
  enum: "#fb923c",
  interface: "#facc15",
  impl: "#a5b4fc",
  default: "#8c93a5",
};

function formatName(name) {
  return name
    .split("::")
    .map(part => `<span class="syntax-ident">${part}</span>`)
    .join(`<span class="syntax-key">::</span>`);
}

function renderSig(kind, name) {
  if (kind === "impl") {
    const clean = name.replace(/^impl\s+/, "").trim();
    const m = /^(.+)\s+for\s+(.+)$/.exec(clean);
    if (m) {
      return `<span class="syntax-key">impl</span> ${formatName(m[1].trim())} <span class="syntax-key">for</span> ${formatName(m[2].trim())}`;
    }
    return `<span class="syntax-key">impl</span> ${formatName(clean)}`;
  }
  return `<span class="syntax-key">${kind}</span> ${formatName(name)}`;
}

function displayName(kind, name) {
  if (kind === "impl") {
    return name.replace(/^impl\s+/, "").trim();
  }
  return name;
}

function computeLayout(canvas, module, ctx) {
  const padding = 32;
  const nodeH = 28;
  const hGap = 18;
  const vGap = 36;
  const imports = (module.imports || []).map((name) => ({ name, kind: "import", signature: name, key: `import:${name}` }));
  const consts = module.items.filter(it => it.kind === "const").map(it => ({ ...it, key: `${it.kind}:${it.name}` }));
  const interfaces = module.items.filter(it => it.kind === "interface").map(it => ({ ...it, key: `${it.kind}:${it.name}` }));
  const structs = module.items.filter(it => it.kind === "struct").map(it => ({ ...it, key: `${it.kind}:${it.name}` }));
  const impls = module.items.filter(it => it.kind === "impl").map(it => ({ ...it, key: `${it.kind}:${it.name}` }));
  const fns = module.items.filter(it => it.kind === "fn").map(it => ({ ...it, key: `${it.kind}:${it.name}` }));
  const macros = module.items.filter(it => it.kind === "macro").map(it => ({ ...it, key: `${it.kind}:${it.name}` }));

  const layers = [
    imports,
    [...consts, ...interfaces, ...structs],
    impls,
    [...fns, ...macros],
  ];

  const layerCols = layers.map((layer, idx) => {
    if (idx === layers.length - 1) {
      return Math.max(1, Math.min(3, Math.ceil(layer.length / 6)));
    }
    if (idx === 2) { // impl layer
      return Math.min(2, Math.max(1, Math.ceil(layer.length / 2)));
    }
    return Math.max(1, Math.ceil(Math.sqrt(layer.length || 1)));
  });

  const neededHeight =
    padding * 2 +
    layers.reduce((acc, layer, i) => {
      const cols = layerCols[i];
      const rows = Math.max(1, Math.ceil((layer.length || 1) / cols));
      return acc + rows * (nodeH + hGap) + vGap;
    }, 0);
  if (canvas.height < neededHeight) {
    canvas.height = neededHeight;
  }

  const nodes = [];
  let yCursor = padding;
  const width = canvas.width;
  layers.forEach((layer, idx) => {
    const cols = layerCols[idx];
    const colWidth = (width - padding * 2) / cols;
    layer.forEach((it, iidx) => {
      const col = iidx % cols;
      const row = Math.floor(iidx / cols);
      const x = padding + col * colWidth + colWidth * 0.1;
      const y = yCursor + row * (nodeH + hGap);
      const boxW = colWidth * 0.8;
      const display = displayName(it.kind, it.name);
      const label = trimText(ctx, `${it.kind} ${display}`, boxW - 14);
      nodes.push({
        key: it.key,
        kind: it.kind,
        name: display,
        x,
        y,
        w: boxW,
        h: nodeH,
        label,
      });
    });
    const rows = Math.max(1, Math.ceil((layer.length || 1) / cols));
    yCursor += rows * (nodeH + hGap) + vGap;
  });

  return { nodes, edges: module.edges || [], layerCols };
}

function drawGraph(ctx, layout, highlightNodes = new Set(), highlightEdges = new Set()) {
  const canvas = ctx.canvas;
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  ctx.font = "13px 'JetBrains Mono', monospace";
  ctx.fillStyle = "#e8ecf2";
  ctx.fillText(canvas.dataset.title || "", 32, 24);

  const nodeLookup = new Map();
  layout.nodes.forEach(n => nodeLookup.set(n.key, n));

  // Edges
  const degrees = new Map();
  layout.nodes.forEach(n => degrees.set(n.key, { in: 0, out: 0 }));
  layout.edges.forEach(edge => {
    const dFrom = degrees.get(edge.from);
    const dTo = degrees.get(edge.to);
    if (dFrom) dFrom.out += 1;
    if (dTo) dTo.in += 1;
  });
  function anchor(node, side) {
    const d = degrees.get(node.key) || { in: 0, out: 0 };
    const base = 5;
    const r = base + Math.min(4, side === "left" ? d.in : d.out);
    const cx = side === "left" ? node.x + r + 4 : node.x + node.w - r - 4;
    const cy = node.y + node.h / 2;
    return { cx, cy, r };
  }

  layout.edges.forEach(edge => {
    const from = nodeLookup.get(edge.from);
    const to = nodeLookup.get(edge.to);
    if (!from || !to) return;
    const active = highlightEdges.has(edge) || (highlightNodes.size > 0 && highlightNodes.has(edge.from) && highlightNodes.has(edge.to));
    const alpha = active ? 0.9 : 0.2;
    const { cx: fx, cy: fy, r: fr } = anchor(from, "right");
    const { cx: tx, cy: ty, r: tr } = anchor(to, "left");
    ctx.strokeStyle = `rgba(255,255,255,${alpha})`;
    ctx.lineWidth = active ? 2 : 1;
    ctx.lineCap = "round";
    ctx.beginPath();
    ctx.moveTo(fx, fy);
    ctx.lineTo(tx, ty);
    ctx.stroke();
    ctx.fillStyle = `rgba(255,255,255,${alpha})`;
    ctx.beginPath();
    ctx.arc(fx, fy, fr * 0.5, 0, Math.PI * 2);
    ctx.fill();
    ctx.beginPath();
    ctx.arc(tx, ty, tr * 0.5, 0, Math.PI * 2);
    ctx.fill();
  });

  // Nodes
  layout.nodes.forEach(node => {
    const active = highlightNodes.size === 0 || highlightNodes.has(node.key);
    const color = colors[node.kind] || colors.default;
    const alpha = active ? 1 : 0.25;
    ctx.globalAlpha = alpha;
    // pill
    ctx.fillStyle = `${color}20`;
    ctx.strokeStyle = color;
    ctx.lineWidth = active ? 2 : 1;
    const r = node.h / 2;
    ctx.beginPath();
    ctx.moveTo(node.x + r, node.y);
    ctx.lineTo(node.x + node.w - r, node.y);
    ctx.arc(node.x + node.w - r, node.y + r, r, -Math.PI / 2, Math.PI / 2);
    ctx.lineTo(node.x + r, node.y + node.h);
    ctx.arc(node.x + r, node.y + r, r, Math.PI / 2, -Math.PI / 2);
    ctx.closePath();
    ctx.fill();
    ctx.stroke();
    // connectors
    const { cx: lx, cy: ly, r: lr } = anchor(node, "left");
    const { cx: rx, cy: ry, r: rr } = anchor(node, "right");
    const deg = degrees.get(node.key) || { in: 0, out: 0 };
    ctx.fillStyle = color;
    if (deg.in > 0) {
      ctx.beginPath(); ctx.arc(lx, ly, lr, 0, Math.PI * 2); ctx.fill();
    }
    if (deg.out > 0) {
      ctx.beginPath(); ctx.arc(rx, ry, rr, 0, Math.PI * 2); ctx.fill();
    }
    ctx.globalAlpha = 1;
    // keyword + name centered
    ctx.textBaseline = "middle";
    const keyColor = "#b983ff";
    const nameColor = "#7dd3fc";
    const keyText = node.kind;
    const nameText = node.name;
    const space = ctx.measureText(' ').width;
    const keyWidth = ctx.measureText(keyText).width;
    const nameWidth = ctx.measureText(nameText).width;
    const total = keyWidth + space + nameWidth;
    const startX = node.x + node.w / 2 - total / 2;
    ctx.fillStyle = active ? keyColor : "rgba(185,131,255,0.55)";
    ctx.fillText(keyText, startX, node.y + node.h / 2);
    if (node.kind === "impl" && nameText.includes(" for ")) {
      const [lhs, rhs] = nameText.split(/\s+for\s+/);
      const lhsWidth = ctx.measureText(lhs).width;
      const forWidth = ctx.measureText('for').width;
      ctx.fillStyle = active ? nameColor : "rgba(125,211,252,0.55)";
      ctx.fillText(lhs, startX + keyWidth + space, node.y + node.h / 2);
      ctx.fillStyle = active ? keyColor : "rgba(185,131,255,0.55)";
      ctx.fillText('for', startX + keyWidth + space + lhsWidth + space, node.y + node.h / 2);
      ctx.fillStyle = active ? nameColor : "rgba(125,211,252,0.55)";
      ctx.fillText(rhs, startX + keyWidth + space + lhsWidth + space + forWidth + space, node.y + node.h / 2);
    } else {
      ctx.fillStyle = active ? nameColor : "rgba(125,211,252,0.55)";
      ctx.fillText(nameText, startX + keyWidth + space, node.y + node.h / 2);
    }
  });
}

function wireHover(canvas, layout) {
  const ctx = canvas.getContext('2d');
  canvas.dataset.title = canvas.dataset.title || "";
  drawGraph(ctx, layout);

  canvas.onmousemove = (e) => {
    const rect = canvas.getBoundingClientRect();
    const scaleX = canvas.width / rect.width;
    const scaleY = canvas.height / rect.height;
    const x = (e.clientX - rect.left) * scaleX;
    const y = (e.clientY - rect.top) * scaleY;
    let hovered = null;
    for (const node of layout.nodes) {
      if (x >= node.x && x <= node.x + node.w && y >= node.y && y <= node.y + node.h) {
        hovered = node.key;
        break;
      }
    }
    if (!hovered) {
      drawGraph(ctx, layout);
      highlightOutline(null);
      return;
    }
    const related = new Set();
    layout.edges.forEach(edge => {
      if (edge.from === hovered || edge.to === hovered) {
        related.add(edge.from);
        related.add(edge.to);
      }
    });
    related.add(hovered);
    drawGraph(ctx, layout, related);
    highlightOutline(hovered);
  };
  canvas.onmouseleave = () => {
    drawGraph(ctx, layout);
    highlightOutline(null);
  };
}

function wireClicks(canvas, layout) {
  canvas.onclick = (e) => {
    const rect = canvas.getBoundingClientRect();
    const scaleX = canvas.width / rect.width;
    const scaleY = canvas.height / rect.height;
    const x = (e.clientX - rect.left) * scaleX;
    const y = (e.clientY - rect.top) * scaleY;
    let target = null;
    for (const node of layout.nodes) {
      if (x >= node.x && x <= node.x + node.w && y >= node.y && y <= node.y + node.h) {
        target = node;
        break;
      }
    }
    if (!target) return;
    const detailCards = Array.from(document.querySelectorAll('.content .card'));
    const match = detailCards.find(card => card.dataset.key === target.key || card.querySelector('.sig')?.textContent?.includes(target.name));
    if (match) {
      match.scrollIntoView({ behavior: 'smooth', block: 'start' });
      return;
    }
    // If import, try to scroll to matching outline/link
    if (target.key.startsWith("import:")) {
      const importName = target.key.split(":")[1];
      const link = Array.from(document.querySelectorAll('.outline a')).find(a => (a.dataset.key || '') === target.key);
      if (link) {
        link.click();
        link.scrollIntoView({ behavior: 'smooth', block: 'start' });
      }
    }
  };
}

function highlightOutline(key) {
  const links = document.querySelectorAll('.outline a');
  links.forEach(link => {
    const matches = key && link.dataset.key === key;
    link.style.background = matches ? "rgba(99,164,255,0.15)" : "transparent";
    // keep syntax color from class; reset via inline color
    link.style.color = matches ? "#e8ecf2" : "";
  });
}

function trimText(ctx, text, maxWidth) {
  if (ctx.measureText(text).width <= maxWidth) return text;
  let trimmed = text;
  while (trimmed.length > 3 && ctx.measureText(trimmed + '…').width > maxWidth) {
    trimmed = trimmed.slice(0, -1);
  }
  return trimmed + '…';
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
  expanded.clear();
  renderAll('');
  searchClear.classList.remove('visible');
});
searchEl.addEventListener('input', () => {
  if (searchEl.value) {
    searchClear.classList.add('visible');
  } else {
    searchClear.classList.remove('visible');
  }
});
function applyState(state) {
  suppressPush = true;
  activeModule = state.module || 0;
  renderAll(filterTerm);
  if (state.anchor) {
    const el = document.getElementById(state.anchor);
    if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }
  suppressPush = false;
}
function stateFromHash() {
  const h = decodeURIComponent(location.hash.slice(1));
  if (!h) return null;
  const sep = h.lastIndexOf(':');
  let name = h;
  let anchor = null;
  if (sep !== -1) {
    name = h.slice(0, sep);
    anchor = h.slice(sep + 1);
  }
  const idx = modules.findIndex(m => m.name === name);
  if (idx >= 0) return { module: idx, anchor: anchor || null };
  return null;
}
window.addEventListener('popstate', (e) => {
  const st = e.state;
  if (st) {
    applyState(st);
  }
});
const initial = history.state || stateFromHash() || { module: 0, anchor: null };
applyState(initial);
pushState(initial.module, initial.anchor);
</script>
"##,
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
