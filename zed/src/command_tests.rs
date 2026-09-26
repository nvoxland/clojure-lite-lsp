use super::command::*;

fn strings(xs: &[&str]) -> Vec<String> {
    xs.iter().map(|s| s.to_string()).collect()
}

#[test]
fn finds_csl_on_path_and_runs_it_as_a_language_server() {
    assert_eq!(
        resolve(None, Some("/usr/local/bin/csl".into())),
        Ok(Launch {
            command: "/usr/local/bin/csl".into(),
            args: strings(&["lsp"]),
            env: vec![],
        })
    );
}

#[test]
fn settings_path_wins_over_path_lookup() {
    let o = Override {
        path: Some("/opt/csl/target/csl".into()),
        arguments: None,
        env: vec![],
    };
    assert_eq!(
        resolve(Some(o), Some("/usr/local/bin/csl".into())),
        Ok(Launch {
            command: "/opt/csl/target/csl".into(),
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
        env: vec![("CSL_HOME".into(), "/tmp/csl-home".into())],
    };
    assert_eq!(
        resolve(Some(o), Some("/bin/csl".into())),
        Ok(Launch {
            command: "/bin/csl".into(),
            args: strings(&["lsp"]),
            env: vec![("CSL_HOME".into(), "/tmp/csl-home".into())],
        })
    );
}

#[test]
fn not_found_explains_how_to_configure() {
    let err = resolve(None, None).unwrap_err();
    assert!(err.contains("csl"), "{err}");
    assert!(err.contains("PATH"), "{err}");
    assert!(err.contains("lsp.clojure-sqlite-lsp.binary.path"), "{err}");
}
