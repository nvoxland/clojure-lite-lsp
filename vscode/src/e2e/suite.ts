// Runs inside VS Code: open a file, ask for definitions and hover.
import * as assert from "node:assert/strict";
import * as path from "node:path";
import * as vscode from "vscode";

async function eventually<T>(f: () => Promise<T | undefined>, ms = 120000): Promise<T> {
  const end = Date.now() + ms;
  for (;;) {
    const v = await f();
    if (v !== undefined) return v;
    if (Date.now() > end) throw new Error("timed out");
    await new Promise((r) => setTimeout(r, 500));
  }
}

export async function run(): Promise<void> {
  const root = vscode.workspace.workspaceFolders![0].uri.fsPath;
  const b = await vscode.workspace.openTextDocument(path.join(root, "src", "app", "b.clj"));
  await vscode.window.showTextDocument(b);
  assert.equal(b.languageId, "clojure");
  const call = new vscode.Position(4, b.lineAt(4).text.indexOf("greet"));

  // indexing starts when the server does: wait for the answer
  const defs = await eventually(async () => {
    const r = await vscode.commands.executeCommand<(vscode.Location | vscode.LocationLink)[]>(
      "vscode.executeDefinitionProvider", b.uri, call);
    return r && r.length > 0 ? r : undefined;
  });
  const target = "targetUri" in defs[0] ? defs[0].targetUri : defs[0].uri;
  assert.equal(path.basename(target.fsPath), "a.clj");

  const hovers = await vscode.commands.executeCommand<vscode.Hover[]>("vscode.executeHoverProvider", b.uri, call);
  const text = hovers.flatMap((h) => h.contents.map((c) => (typeof c === "string" ? c : c.value))).join("\n");
  assert.match(text, /Says hello/);
  console.log("e2e: definition and hover answered");
}
