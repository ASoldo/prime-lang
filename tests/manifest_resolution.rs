use prime_lang::project::{
    deps::slug,
    lock::{load_lockfile, require_frozen},
    manifest::PackageManifest,
};
use serde_json::Value;
use std::{
    fs,
    path::Path,
    process::{Command, Output},
};
use tempfile::tempdir;

fn package(dir: &Path, dependencies: &str) {
    fs::create_dir_all(dir).unwrap();
    let name = dir.file_name().unwrap().to_str().unwrap();
    fs::write(dir.join("prime.toml"), format!(
        "manifest_version='3'\n[package]\nname='{name}'\n[module]\nname='{name}'\npath='main.prime'\n[dependencies]\n{dependencies}\n"
    )).unwrap();
    fs::write(
        dir.join("main.prime"),
        format!("module {name}; fn main() {{ out(7); }}"),
    )
    .unwrap();
}

fn cli(dir: &Path, args: &[&str], code: i32) -> Output {
    let output = Command::new(env!("CARGO_BIN_EXE_prime-lang"))
        .args(args)
        .current_dir(dir)
        .env("GIT_ALLOW_PROTOCOL", "file")
        .env_remove("PRIME_TARGET")
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(code), "{args:?}: {output:?}");
    output
}

fn diagnostic(dir: &Path, frozen: bool) -> String {
    let mut args = vec!["check", "main.prime", "--format=json"];
    if frozen {
        args.push("--frozen");
    }
    let output = cli(dir, &args, 1);
    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    let first = &report["diagnostics"][0];
    assert_eq!(first["phase"], "manifest", "{report}");
    first["message"].as_str().unwrap().to_owned()
}

fn git(dir: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .args([
            "-c",
            "user.name=Prime Test",
            "-c",
            "user.email=prime@example.invalid",
            "-c",
            "commit.gpgsign=false",
        ])
        .args(args)
        .current_dir(dir)
        .env("GIT_ALLOW_PROTOCOL", "file")
        .output()
        .unwrap();
    assert!(output.status.success(), "{args:?}: {output:?}");
    String::from_utf8(output.stdout).unwrap().trim().to_owned()
}

#[test]
fn shared_dependency_is_parsed_once_and_lookups_use_its_snapshot() {
    let tmp = tempdir().unwrap();
    let root = tmp.path().join("root");
    let shared = tmp.path().join("shared");
    package(&shared, "");
    package(&tmp.path().join("left"), "shared={path='../shared'}");
    package(&tmp.path().join("right"), "shared={path='../shared/./'}");
    package(&root, "left={path='../left'}\nright={path='../right'}");
    let manifest = PackageManifest::load(&root.join("prime.toml")).unwrap();
    let leaf = |name| {
        manifest
            .dependency_manifest(name)
            .unwrap()
            .dependency_manifest("shared")
            .unwrap()
    };
    assert!(std::ptr::eq(leaf("left"), leaf("right")));
    assert_eq!(manifest.module_entries().len(), 4);
    let initial = manifest.module_path("shared").unwrap();
    fs::write(
        shared.join("prime.toml"),
        "[libraries]\nsurprise='main.prime'\n",
    )
    .unwrap();
    assert_eq!(manifest.module_path("shared"), Some(initial));
    assert!(manifest.module_path("surprise").is_none());
}

#[test]
fn cycle_and_malformed_or_missing_local_manifests_fail_with_context() {
    let tmp = tempdir().unwrap();
    let app = tmp.path().join("app");
    let dep = tmp.path().join("dep");
    package(&app, "dep={path='../dep'}");
    package(&dep, "app={path='../app/../app'}");
    let error = diagnostic(&app, false);
    assert!(error.contains("dependency cycle"), "{error}");
    for path in [&dep, &app] {
        assert!(
            error.contains(path.join("prime.toml").to_str().unwrap()),
            "{error}"
        );
    }
    let dep_manifest = dep.join("prime.toml").canonicalize().unwrap();
    fs::write(&dep_manifest, "[broken").unwrap();
    assert!(diagnostic(&app, false).contains(dep_manifest.to_str().unwrap()));
    fs::write(&dep_manifest, "[module]\nname='broken'").unwrap();
    assert!(diagnostic(&app, false).contains(dep_manifest.to_str().unwrap()));
    fs::remove_file(dep.join("prime.toml")).unwrap();
    assert!(diagnostic(&app, false).contains("prime.toml"));
}

#[test]
fn shipped_dotted_names_and_equivalent_path_sources_work_but_drift_fails_all_commands() {
    let shipped = Path::new(env!("CARGO_MANIFEST_DIR")).join("workspace/app/prime.toml");
    let manifest = PackageManifest::load(&shipped).unwrap();
    require_frozen(Some(&manifest), &shipped).unwrap();
    let tmp = tempdir().unwrap();
    let app = tmp.path().join("app");
    package(
        &app,
        "lib={name='util::lib',path='../dep',features=['a','b']}",
    );
    package(&tmp.path().join("dep"), "");
    package(&tmp.path().join("wrong"), "");
    let lock = app.join("prime.lock");
    let good = "[[dependencies]]\nname='util.lib'\npath='../dep/.'\nfeatures=['b','a']\n";
    fs::write(&lock, good).unwrap();
    cli(&app, &["check", "main.prime", "--frozen"], 0);
    fs::write(&lock, good.replace("../dep/.", "../wrong")).unwrap();
    for command in ["check", "run", "build"] {
        let output = cli(&app, &[command, "main.prime", "--frozen"], 1);
        let message = String::from_utf8(output.stderr).unwrap();
        assert!(
            message.contains("util::lib") && message.contains("locked path"),
            "{message}"
        );
    }
    assert!(!app.join(".build.prime").exists());
    for (bad, expected) in [
        ("dependencies=[]".into(), "missing lock entry"),
        (format!("{good}\n{good}"), "duplicate lock entry"),
        (
            format!("{good}\n[[dependencies]]\nname='old'\npath='../dep'"),
            "undeclared",
        ),
        (good.replace("'b','a'", "'a'"), "features"),
        (good.replace("path=", "git="), "source kind"),
    ] {
        fs::write(&lock, bad).unwrap();
        assert!(diagnostic(&app, true).contains(expected));
    }
}

#[test]
fn frozen_validates_transitive_lockfiles_and_exempts_leaves() {
    let tmp = tempdir().unwrap();
    let app = tmp.path().join("app");
    let middle = tmp.path().join("middle");
    package(&app, "middle={path='../middle'}");
    package(&middle, "leaf={path='../leaf'}");
    package(&tmp.path().join("leaf"), "");
    cli(&app, &["sync"], 0);
    let output = cli(
        &app,
        &["check", "main.prime", "--frozen", "--format=json"],
        1,
    );
    let report: Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(
        report["diagnostics"][0]["path"],
        serde_json::json!(middle.canonicalize().unwrap().join("prime.lock"))
    );
    cli(&middle, &["sync"], 0);
    cli(&app, &["check", "main.prime", "--frozen"], 0);
}

#[test]
fn frozen_compares_local_git_head_source_and_tracked_files_offline() {
    let tmp = tempdir().unwrap();
    let app = tmp.path().join("app");
    let upstream = tmp.path().join("upstream");
    package(&app, "");
    package(&upstream, "");
    git(&upstream, &["init", "-q"]);
    git(&upstream, &["add", "."]);
    git(&upstream, &["commit", "-qm", "initial"]);
    let old_rev = git(&upstream, &["rev-parse", "HEAD"]);
    cli(&app, &["add", "dep", "--git", "../upstream"], 0);
    let cache = app.join(".prime/deps").join(slug("dep", "../upstream"));
    let lock = app.join("prime.lock");
    let old_lock = fs::read_to_string(&lock).unwrap();
    fs::rename(&upstream, tmp.path().join("upstream-offline")).unwrap();
    cli(&app, &["check", "main.prime", "--frozen"], 0);
    cli(&app, &["sync"], 0);
    assert_eq!(
        load_lockfile(&lock).unwrap().dependencies[0].rev.as_deref(),
        Some(old_rev.as_str())
    );

    fs::write(&lock, old_lock.replace("../upstream", "../other")).unwrap();
    assert!(diagnostic(&app, true).contains("Git source"));
    fs::write(&lock, &old_lock).unwrap();
    git(&cache, &["remote", "set-url", "origin", "../other"]);
    assert!(diagnostic(&app, true).contains("checkout origin"));
    git(
        &cache,
        &["remote", "set-url", "origin", upstream.to_str().unwrap()],
    );

    fs::write(
        cache.join("main.prime"),
        "module upstream; fn main() { out(8); }",
    )
    .unwrap();
    assert!(diagnostic(&app, true).contains("modified tracked files"));
    git(&cache, &["add", "."]);
    git(&cache, &["commit", "-qm", "new revision"]);
    let new_rev = git(&cache, &["rev-parse", "HEAD"]);
    let drift = diagnostic(&app, true);
    assert!(
        drift.contains(&old_rev) && drift.contains(&new_rev),
        "{drift}"
    );
    cli(&app, &["sync"], 0);
    cli(&app, &["check", "main.prime", "--frozen"], 0);

    fs::rename(&cache, tmp.path().join("saved-checkout")).unwrap();
    cli(&app, &["sync"], 0);
    assert_eq!(
        load_lockfile(&lock).unwrap().dependencies[0].rev.as_deref(),
        Some(new_rev.as_str())
    );
    assert!(diagnostic(&app, true).contains("missing Git checkout"));
}

#[test]
fn add_and_sync_can_record_a_git_source_before_checkout() {
    let tmp = tempdir().unwrap();
    let app = tmp.path().join("app");
    package(&app, "");
    cli(&app, &["add", "pending", "--git", "../missing-origin"], 0);
    let pending_cache = app
        .join(".prime/deps")
        .join(slug("pending", "../missing-origin"));
    fs::create_dir_all(pending_cache).unwrap();
    cli(&app, &["sync"], 0);
    let lock = load_lockfile(&app.join("prime.lock")).unwrap();
    assert_eq!(
        lock.dependencies[0].git.as_deref(),
        Some("../missing-origin")
    );
    assert!(lock.dependencies[0].rev.is_none());
    assert!(diagnostic(&app, true).contains("missing locked Git revision"));
}

#[test]
fn fetch_locked_restores_pinned_graph_and_reuses_it_offline() {
    let tmp = tempdir().unwrap();
    let app = tmp.path().join("app");
    let leaf = tmp.path().join("leaf");
    let middle = tmp.path().join("middle");
    for dir in [&leaf, &middle, &app] {
        package(dir, "");
    }
    git(&leaf, &["init", "-q"]);
    git(&leaf, &["add", "."]);
    git(&leaf, &["commit", "-qm", "leaf A"]);
    let leaf_a = git(&leaf, &["rev-parse", "HEAD"]);
    let leaf_url = format!("file://{}", leaf.display());
    cli(&middle, &["add", "leaf", "--git", &leaf_url], 0);
    let middle_lock = fs::read(middle.join("prime.lock")).unwrap();
    git(&middle, &["init", "-q"]);
    git(&middle, &["add", "prime.toml", "prime.lock", "main.prime"]);
    git(&middle, &["commit", "-qm", "middle A"]);
    let middle_a = git(&middle, &["rev-parse", "HEAD"]);
    cli(&app, &["add", "middle", "--git", "../middle"], 0);
    let lock_path = app.join("prime.lock");
    let app_lock = fs::read_to_string(&lock_path).unwrap();
    for dir in [&leaf, &middle] {
        git(
            dir,
            &["commit", "--allow-empty", "-qm", "advance remote to B"],
        );
    }
    fs::remove_dir_all(app.join(".prime")).unwrap();
    let middle_cache = app.join(".prime/deps").join(slug("middle", "../middle"));
    for (bad, message) in [
        (app_lock.replace("../middle", "../other"), "Git source"),
        (app_lock.replace(&middle_a, "HEAD"), "commit ID"),
        (app_lock.replace(&middle_a, &"1".repeat(40)), "fetch"),
    ] {
        fs::write(&lock_path, &bad).unwrap();
        let output = cli(&app, &["fetch", "--locked"], 1);
        assert!(String::from_utf8_lossy(&output.stderr).contains(message));
        assert_eq!(fs::read_to_string(&lock_path).unwrap(), bad);
        assert!(!middle_cache.exists());
    }
    fs::write(&lock_path, &app_lock).unwrap();
    cli(tmp.path(), &["fetch", "--locked", "app"], 0);
    let leaf_cache = middle_cache
        .join(".prime/deps")
        .join(slug("leaf", &leaf_url));
    assert_eq!(git(&middle_cache, &["rev-parse", "HEAD"]), middle_a);
    assert_eq!(git(&leaf_cache, &["rev-parse", "HEAD"]), leaf_a);
    fs::rename(&leaf, tmp.path().join("offline-leaf")).unwrap();
    fs::rename(&middle, tmp.path().join("offline-middle")).unwrap();
    cli(&app, &["fetch", "--locked"], 0);
    cli(&app, &["check", "main.prime", "--frozen"], 0);
    assert_eq!(fs::read_to_string(&lock_path).unwrap(), app_lock);
    assert_eq!(
        fs::read(middle_cache.join("prime.lock")).unwrap(),
        middle_lock
    );
    git(
        &leaf_cache,
        &["commit", "--allow-empty", "-qm", "unexpected HEAD"],
    );
    let err = cli(&app, &["fetch", "--locked"], 1);
    assert!(String::from_utf8_lossy(&err.stderr).contains(&leaf_a));
    git(&leaf_cache, &["checkout", "--detach", &leaf_a]);
    git(&leaf_cache, &["remote", "set-url", "origin", "../wrong"]);
    let err = cli(&app, &["fetch", "--locked"], 1);
    assert!(String::from_utf8_lossy(&err.stderr).contains("checkout origin"));
}
