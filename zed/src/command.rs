//! How the language server is launched, decided from local configuration only
//! (Zed settings or PATH). Kept free of zed types so it can be unit tested.

/// The language server's id in Zed (settings live under `lsp.<SERVER_NAME>`).
pub const SERVER_NAME: &str = "clojure-sqlite-lsp";

/// The executable looked up on PATH.
pub const BINARY_NAME: &str = "csl";

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
/// Arguments default to `csl lsp`; settings can replace them. Env from settings
/// applies either way.
pub fn resolve(override_: Option<Override>, which: Option<String>) -> Result<Launch, String> {
    let o = override_.unwrap_or_default();
    let command = o.path.or(which).ok_or_else(|| {
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
        env: o.env,
    })
}
