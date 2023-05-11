"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const vscode = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    const serverOptions = {
        run: {
            command: 'pedant',
            args: ['lsp'],
            transport: node_1.TransportKind.stdio
        },
        debug: {
            command: 'pedant',
            args: ['lsp'],
            transport: node_1.TransportKind.stdio
        }
    };
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'pedant' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.ped')
        }
    };
    client = new node_1.LanguageClient('pedantLanguageServer', 'Pedant Language Server', serverOptions, clientOptions);
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map