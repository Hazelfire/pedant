{-# LANGUAGE OverloadedStrings #-}

module LSP (runLSP) where

import Control.Lens hiding (Iso)
import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.LSP.Diagnostics hiding (LSP)
import Language.LSP.Server hiding (LSP)
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSPL
import System.Log.Logger

handlers :: (String -> IO [LSP.Diagnostic]) -> Handlers (LspM ())
handlers getErrors =
  mconcat
    [ notificationHandler LSP.SInitialized $ \_msg -> do
        liftIO $ debugM "reactor.handle" "Processing the Initialized notification"

        -- We're initialized! Lets send a showMessageRequest now
        sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "Initializing")

        -- We can dynamically register a capability once the user accepts it
        sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "Turning on code lenses dynamically")

        let regOpts = LSP.CodeLensRegistrationOptions Nothing Nothing (Just False)

        void $
          registerCapability LSP.STextDocumentCodeLens regOpts $ \_req responder -> do
            liftIO $ debugM "reactor.handle" "Processing a textDocument/codeLens request"
            let cmd = LSP.Command "Say hello" "lsp-ped-hello" Nothing
                rsp =
                  LSP.List
                    [ LSP.CodeLens (LSP.mkRange 0 0 0 100) (Just cmd) Nothing,
                      LSP.CodeLens (LSP.mkRange 1 0 0 100) (Just cmd) Nothing
                    ]
            responder (Right rsp),
      notificationHandler LSP.STextDocumentDidSave $ \msg -> do
        let doc = msg ^. LSPL.params . LSPL.textDocument . LSPL.uri
            fileName = LSP.uriToFilePath doc
        sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "file changed")
        liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
        case fileName of
          Just name -> do
            diagnostics <- liftIO (getErrors name)
            publishDiagnostics 100 (LSP.toNormalizedUri doc) (Just 0) (partitionBySource diagnostics)
          _ -> pure (),
      notificationHandler LSP.STextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSPL.params . LSPL.textDocument . LSPL.uri
            fileName = LSP.uriToFilePath doc
        sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "Checking for errors")
        liftIO $ debugM "reactor.handle" $ "Processing DidOpen  for: " ++ show fileName
        case fileName of
          Just name -> do
            diagnostics <- liftIO (getErrors name)
            publishDiagnostics 100 (LSP.toNormalizedUri doc) (Just 0) (partitionBySource diagnostics)
          _ -> pure ()
    ]

runLSP :: (String -> IO [LSP.Diagnostic]) -> IO Int
runLSP getErrors =
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ const $ Right (),
        defaultConfig = (),
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = handlers getErrors,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = lspOptions
      }

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    (Just True)
    (Just LSP.TdSyncIncremental)
    (Just False)
    (Just False)
    (Just $ LSP.InR $ LSP.SaveOptions $ Just False)

lspOptions :: Options
lspOptions =
  defaultOptions
    { textDocumentSync = Just syncOptions,
      executeCommandCommands = Just ["lsp-ped-command"]
    }
