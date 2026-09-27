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
  const highlights = await vscode.commands.executeCommand<vscode.DocumentHighlight[]>(
    "vscode.executeDocumentHighlights", b.uri, call);
  assert.equal(highlights.length, 1);

  // typing a new call: argument hints come from the index by name
  const editor = vscode.window.activeTextEditor!;
  const end = b.lineAt(b.lineCount - 1).range.end;
  await editor.edit((e) => e.insert(end, "\n(a/greet "));
  const help = await vscode.commands.executeCommand<vscode.SignatureHelp>(
    "vscode.executeSignatureHelpProvider", b.uri, b.lineAt(b.lineCount - 1).range.end);
  assert.equal(help.signatures[0].label, "greet [who]");

  // renaming a local: every use, in this file
  const a = await vscode.workspace.openTextDocument(path.join(root, "src", "app", "a.clj"));
  const who = new vscode.Position(5, a.lineAt(5).text.indexOf("who"));
  const edit = await vscode.commands.executeCommand<vscode.WorkspaceEdit>(
    "vscode.executeDocumentRenameProvider", a.uri, who, "person");
  assert.equal(edit.get(a.uri).length, 2);
  await assert.rejects(
    Promise.resolve(vscode.commands.executeCommand("vscode.executeDocumentRenameProvider", b.uri, call, "hi")),
    "a var isn't renamed");
  console.log("e2e: definition, hover, highlights, signature help and rename answered");
}
