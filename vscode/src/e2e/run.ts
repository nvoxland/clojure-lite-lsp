// End to end: a real VS Code (downloaded into .vscode-test, with its own
// profile) runs this extension against the installed clojure-lite-lsp on a
// small project, with a throwaway index.
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { runTests } from "@vscode/test-electron";

async function main(): Promise<void> {
  const tmp = fs.realpathSync(fs.mkdtempSync(path.join(os.tmpdir(), "clojure-lite-lsp-e2e-")));
  const project = path.join(tmp, "project");
  fs.mkdirSync(path.join(project, "src", "app"), { recursive: true });
  fs.writeFileSync(path.join(project, "deps.edn"), '{:paths ["src"]}');
  fs.writeFileSync(path.join(project, "src", "app", "a.clj"), '(ns app.a)\n\n(defn greet\n  "Says hello."\n  [who]\n  (str "hello " who))\n');
  fs.writeFileSync(path.join(project, "src", "app", "b.clj"), '(ns app.b\n  (:require [app.a :as a]))\n\n(defn main []\n  (a/greet "you"))\n');
  process.env.CLOJURE_LITE_LSP_HOME = path.join(tmp, "home");
  await runTests({
    extensionDevelopmentPath: path.resolve(__dirname, "..", ".."),
    extensionTestsPath: path.resolve(__dirname, "suite"),
    // short: VS Code puts a socket in it, and macOS caps socket paths at ~100 bytes
    launchArgs: [project, "--disable-extensions", "--user-data-dir", fs.mkdtempSync("/tmp/cll-")],
  });
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
