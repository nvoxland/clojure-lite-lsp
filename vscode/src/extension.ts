// Starts clojure-lite-lsp for Clojure files. VS Code's built-in Clojure support
// provides the language (syntax highlighting); this adds the server.
import * as fs from "node:fs";
import * as os from "node:os";
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
    launch = resolve(settings(), findOnPath(process.env.PATH, isFile), os.homedir());
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
  const c = new LanguageClient(BINARY_NAME, BINARY_NAME, serverOptions, clientOptions);
  try {
    await c.start();
    client = c;
  } catch (e) {
    vscode.window.showErrorMessage(`${BINARY_NAME} didn't start (${launch.command}): ${(e as Error).message}`);
  }
}

// restarts take turns: two at once would each start a server
let restarting: Promise<void> = Promise.resolve();

function restart(context: vscode.ExtensionContext): Promise<void> {
  restarting = restarting.then(async () => {
    const old = client;
    client = undefined;
    await old?.stop().catch(() => undefined);
    await start(context);
  });
  return restarting;
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  context.subscriptions.push(
    vscode.commands.registerCommand("clojure-lite-lsp.restart", () => restart(context)),
    vscode.workspace.onDidChangeConfiguration((e) => {
      if (e.affectsConfiguration(BINARY_NAME)) {
        return restart(context);
      }
    })
  );
  await restart(context);
}

export async function deactivate(): Promise<void> {
  await restarting;
  await client?.stop();
}
