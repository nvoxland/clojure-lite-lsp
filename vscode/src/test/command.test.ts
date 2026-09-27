// How the server is launched, decided from settings and PATH only: kept free
// of the vscode API so it runs under plain node.
import { test } from "node:test";
import * as assert from "node:assert/strict";
import * as path from "node:path";
import { resolve, findOnPath } from "../command";

const exists = (files: string[]) => (p: string) => files.includes(p);

test("finds clojure-lite-lsp on PATH and runs it as a language server", () => {
  const found = path.join("/usr/local/bin", "clojure-lite-lsp");
  assert.deepEqual(
    resolve({}, findOnPath("/usr/bin" + path.delimiter + "/usr/local/bin", exists([found]))),
    { command: found, args: ["lsp"], env: {} }
  );
});

test("the path setting wins over PATH", () => {
  assert.deepEqual(
    resolve({ path: "/opt/cll/target/clojure-lite-lsp" }, "/usr/local/bin/clojure-lite-lsp").command,
    "/opt/cll/target/clojure-lite-lsp"
  );
});

test("an empty path setting means PATH", () => {
  assert.equal(resolve({ path: "  " }, "/bin/clojure-lite-lsp").command, "/bin/clojure-lite-lsp");
});

test("args and env settings are passed through", () => {
  assert.deepEqual(
    resolve({ args: ["lsp"], env: { CLOJURE_LITE_LSP_HOME: "/tmp/try" } }, "/bin/clojure-lite-lsp"),
    { command: "/bin/clojure-lite-lsp", args: ["lsp"], env: { CLOJURE_LITE_LSP_HOME: "/tmp/try" } }
  );
});

test("not found explains how to configure it", () => {
  assert.throws(() => resolve({}, undefined), (e: Error) =>
    e.message.includes("clojure-lite-lsp") && e.message.includes("PATH") && e.message.includes("clojure-lite-lsp.path"));
});

test("PATH lookup skips empty entries and finds nothing when absent", () => {
  assert.equal(findOnPath(path.delimiter + "/nowhere", exists([])), undefined);
  assert.equal(findOnPath(undefined, exists([])), undefined);
});
