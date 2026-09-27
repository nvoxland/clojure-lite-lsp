pub mod command;

#[cfg(test)]
mod command_tests;

use command::{resolve, Override, BINARY_NAME, SERVER_NAME};
use zed_extension_api::{self as zed, settings::LspSettings, LanguageServerId, Result};

struct ClojureSqliteLspExtension;

impl zed::Extension for ClojureSqliteLspExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let override_ = LspSettings::for_worktree(SERVER_NAME, worktree)
            .ok()
            .and_then(|s| s.binary)
            .map(|b| Override {
                path: b.path,
                arguments: b.arguments,
                env: b.env.map(|m| m.into_iter().collect()).unwrap_or_default(),
            });
        let launch = resolve(override_, worktree.which(BINARY_NAME), worktree.shell_env())?;
        Ok(zed::Command {
            command: launch.command,
            args: launch.args,
            env: launch.env,
        })
    }
}

zed::register_extension!(ClojureSqliteLspExtension);
