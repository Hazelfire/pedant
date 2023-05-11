import * as vscode from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions, TransportKind } from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    const serverOptions: ServerOptions = {
        run: {
            command: 'pedant',
            args: ['lsp'],
            transport: TransportKind.stdio
        },
        debug: {
            command: 'pedant',
            args: ['lsp'],
            transport: TransportKind.stdio
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'pedant' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.ped')
        }
    };

    client = new LanguageClient('pedantLanguageServer', 'Pedant Language Server', serverOptions, clientOptions);

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
