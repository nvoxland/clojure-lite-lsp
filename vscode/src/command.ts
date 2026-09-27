// How the language server is launched, decided from settings and PATH only.
// Kept free of the vscode API so it can be tested under plain node.
import * as path from "node:path";

export const BINARY_NAME = "clojure-lite-lsp";
export const DEFAULT_ARGS = ["lsp"];

export interface Settings {
  path?: string;
  args?: string[];
  env?: Record<string, string>;
}

export interface Launch {
  command: string;
  args: string[];
  env: Record<string, string>;
}

/** The clojure-lite-lsp executable on `pathEnv` (a PATH value), if any. */
export function findOnPath(
  pathEnv: string | undefined,
  exists: (file: string) => boolean
): string | undefined {
  const names = process.platform === "win32" ? [BINARY_NAME + ".exe", BINARY_NAME] : [BINARY_NAME];
  for (const dir of (pathEnv ?? "").split(path.delimiter)) {
    if (!dir) continue;
    for (const name of names) {
      const file = path.join(dir, name);
      if (exists(file)) return file;
    }
  }
  return undefined;
}

/**
 * The launch command: the path setting if set, else `which` (the PATH lookup).
 * Arguments default to `clojure-lite-lsp lsp`; env from settings is added to the
 * editor's own.
 */
export function resolve(settings: Settings, which: string | undefined): Launch {
  const command = settings.path?.trim() || which;
  if (!command) {
    throw new Error(
      `${BINARY_NAME} not found on PATH. Install it (see the extension's README) ` +
        `or set clojure-lite-lsp.path in the settings.`
    );
  }
  return { command, args: settings.args ?? DEFAULT_ARGS, env: settings.env ?? {} };
}
