// Starts clojure-lite-lsp for Clojure files. VS Code's built-in Clojure support
// provides the language (syntax highlighting); this adds the server.
import * as fs from "node:fs";
import * as vscode from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";
import { BINARY_NAME, findOnPath, resolve, Settings } from "./command";

let client: LanguageClient | undefined;

function isFile(file: string): boolean {
  try {
    return fs.statSync(file).isFile();
  } catch {
    return false;
  }
}

function settings(): Settings {
  const cfg = vscode.workspace.getConfiguration(BINARY_NAME);
  return {
    path: cfg.get<string>("path"),
    args: cfg.get<string[]>("args"),
    env: cfg.get<Record<string, string>>("env"),
  };
}

async function start(context: vscode.ExtensionContext): Promise<void> {
  let launch;
  try {
    launch = resolve(settings(), findOnPath(process.env.PATH, isFile));
  } catch (e) {
    vscode.window.showErrorMessage((e as Error).message);
    return;
  }
  const serverOptions: ServerOptions = {
    command: launch.command,
    args: launch.args,
    options: { env: { ...process.env, ...launch.env } },
  };
  const clientOptions: LanguageClientOptions = {
    // library sources come back as extracted, read-only files: file scheme too
    documentSelector: [{ scheme: "file", language: "clojure" }],
  };
  client = new LanguageClient(BINARY_NAME, BINARY_NAME, serverOptions, clientOptions);
  await client.start();
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  context.subscriptions.push(
    vscode.commands.registerCommand("clojure-lite-lsp.restart", async () => {
      await client?.stop();
      client = undefined;
      await start(context);
    }),
    vscode.workspace.onDidChangeConfiguration(async (e) => {
      if (e.affectsConfiguration(BINARY_NAME)) {
        await vscode.commands.executeCommand("clojure-lite-lsp.restart");
      }
    })
  );
  await start(context);
}

export async function deactivate(): Promise<void> {
  await client?.stop();
}
