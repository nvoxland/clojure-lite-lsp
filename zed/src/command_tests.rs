use super::command::*;

fn strings(xs: &[&str]) -> Vec<String> {
    xs.iter().map(|s| s.to_string()).collect()
}

#[test]
fn finds_clojure_lite_lsp_on_path_and_runs_it_as_a_language_server() {
    assert_eq!(
        resolve(None, Some("/usr/local/bin/clojure-lite-lsp".into()), vec![]),
        Ok(Launch {
            command: "/usr/local/bin/clojure-lite-lsp".into(),
            args: strings(&["lsp"]),
            env: vec![],
        })
    );
}

#[test]
fn settings_path_wins_over_path_lookup() {
    let o = Override {
        path: Some("/opt/clojure-lite-lsp/target/clojure-lite-lsp".into()),
        arguments: None,
        env: vec![],
    };
    assert_eq!(
        resolve(Some(o), Some("/usr/local/bin/clojure-lite-lsp".into()), vec![]),
        Ok(Launch {
            command: "/opt/clojure-lite-lsp/target/clojure-lite-lsp".into(),
            args: strings(&["lsp"]),
            env: vec![],
        })
    );
}

#[test]
fn settings_arguments_and_env_are_passed_through() {
    // e.g. a separate index while trying things out
    let o = Override {
        path: None,
        arguments: Some(strings(&["lsp"])),
        env: vec![("CLOJURE_LITE_LSP_HOME".into(), "/tmp/clojure-lite-lsp-home".into())],
    };
    assert_eq!(
        resolve(Some(o), Some("/bin/clojure-lite-lsp".into()), vec![]),
        Ok(Launch {
            command: "/bin/clojure-lite-lsp".into(),
            args: strings(&["lsp"]),
            env: vec![("CLOJURE_LITE_LSP_HOME".into(), "/tmp/clojure-lite-lsp-home".into())],
        })
    );
}

#[test]
fn not_found_explains_how_to_configure() {
    let err = resolve(None, None, vec![]).unwrap_err();
    assert!(err.contains("clojure-lite-lsp"), "{err}");
    assert!(err.contains("PATH"), "{err}");
    assert!(err.contains("lsp.clojure-lite-lsp.binary.path"), "{err}");
}

#[test]
fn the_shell_environment_is_passed_on_with_settings_over_it() {
    // Zed started from the Dock has a bare PATH: the indexer the server
    // starts runs `clojure`/`lein` to find classpaths, so it needs the
    // user's shell environment
    let o = Override {
        path: None,
        arguments: None,
        env: vec![("CLOJURE_LITE_LSP_HOME".into(), "/tmp/clojure-lite-lsp-home".into())],
    };
    let shell = vec![
        ("PATH".into(), "/opt/homebrew/bin:/usr/bin".into()),
        ("CLOJURE_LITE_LSP_HOME".into(), "/from/shell".into()),
    ];
    assert_eq!(
        resolve(Some(o), Some("/bin/clojure-lite-lsp".into()), shell).unwrap().env,
        vec![
            ("PATH".to_string(), "/opt/homebrew/bin:/usr/bin".to_string()),
            ("CLOJURE_LITE_LSP_HOME".to_string(), "/tmp/clojure-lite-lsp-home".to_string()),
        ]
    );
}
