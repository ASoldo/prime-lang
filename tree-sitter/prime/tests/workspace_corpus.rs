use std::fs;
use std::path::{Path, PathBuf};
use tree_sitter::{Node, Parser};

fn collect_sources(dir: &Path, paths: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).unwrap_or_else(|e| panic!("{}: {e}", dir.display())) {
        let entry = entry.unwrap();
        let kind = entry.file_type().unwrap();
        let path = entry.path();
        if kind.is_dir() {
            collect_sources(&path, paths);
        } else if kind.is_file() && path.extension().is_some_and(|ext| ext == "prime") {
            paths.push(path);
        }
    }
}

fn first_syntax_error(mut node: Node<'_>) -> Node<'_> {
    while !node.is_error() && !node.is_missing() {
        let mut cursor = node.walk();
        node = node.children(&mut cursor).find(Node::has_error).unwrap();
    }
    node
}

#[test]
fn workspace_corpus() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../workspace");
    let mut paths = Vec::new();
    collect_sources(&workspace, &mut paths);
    paths.sort();
    assert!(
        !paths.is_empty(),
        "empty Prime corpus at {}",
        workspace.display()
    );

    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_prime_lang::LANGUAGE.into())
        .unwrap();
    let mut failures = Vec::new();
    for path in &paths {
        let source = fs::read(path).unwrap();
        let tree = parser.parse(source, None).unwrap();
        if tree.root_node().has_error() {
            let node = first_syntax_error(tree.root_node());
            let point = node.start_position();
            failures.push(format!(
                "{}:{}:{}: {} {}",
                path.strip_prefix(&workspace).unwrap().display(),
                point.row + 1,
                point.column + 1,
                if node.is_missing() {
                    "MISSING"
                } else {
                    "unexpected syntax"
                },
                node.kind(),
            ));
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    println!("Parsed {} workspace .prime files", paths.len());
}
