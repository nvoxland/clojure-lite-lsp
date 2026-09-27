//! How the language server is launched, decided from local configuration only
//! (Zed settings or PATH). Kept free of zed types so it can be unit tested.

/// The language server's id in Zed (settings live under `lsp.<SERVER_NAME>`).
pub const SERVER_NAME: &str = "clojure-lite-lsp";

/// The executable looked up on PATH.
pub const BINARY_NAME: &str = "clojure-lite-lsp";

/// The arguments that start the language server, unless settings say otherwise.
pub const DEFAULT_ARGS: &[&str] = &["lsp"];

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Override {
    pub path: Option<String>,
    pub arguments: Option<Vec<String>>,
    pub env: Vec<(String, String)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Launch {
    pub command: String,
    pub args: Vec<String>,
    pub env: Vec<(String, String)>,
}

/// The launch command: `override_.path` if set, else the PATH lookup `which`.
/// Arguments default to `clojure-lite-lsp lsp`; settings can replace them. The env is the
/// user's shell environment (the indexer the server starts runs `clojure` or
/// `lein`, which a Zed started from the Dock can't find otherwise), with env
/// from settings over it.
pub fn resolve(
    override_: Option<Override>,
    which: Option<String>,
    shell_env: Vec<(String, String)>,
) -> Result<Launch, String> {
    let o = override_.unwrap_or_default();
    // settings often say ~/...: no shell expands it for a spawned process
    let home = shell_env.iter().find(|(k, _)| k == "HOME").map(|(_, v)| v.clone());
    let path = o.path.map(|p| match (&home, p.strip_prefix("~/")) {
        (Some(h), Some(rest)) => format!("{}/{}", h.trim_end_matches('/'), rest),
        _ => p,
    });
    let command = path.or(which).ok_or_else(|| {
        format!(
            "{BINARY_NAME} not found on PATH. Install it (see zed/README.md) \
             or set lsp.{SERVER_NAME}.binary.path in Zed settings."
        )
    })?;
    Ok(Launch {
        command,
        args: o
            .arguments
            .unwrap_or_else(|| DEFAULT_ARGS.iter().map(|s| s.to_string()).collect()),
        env: merge_env(shell_env, o.env),
    })
}

/// `base` with `over`'s values replacing (or adding to) it, in order.
fn merge_env(base: Vec<(String, String)>, over: Vec<(String, String)>) -> Vec<(String, String)> {
    let mut env = base;
    for (k, v) in over {
        match env.iter_mut().find(|(ek, _)| *ek == k) {
            Some(e) => e.1 = v,
            None => env.push((k, v)),
        }
    }
    env
}
