{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Pedant.LSP (runLSP) where

import Control.Lens hiding (Iso)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import qualified Control.Monad.Reader as Reader
import Data.Text qualified as T
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Server qualified as LSP
import Language.LSP.Types qualified as LSP
import Language.LSP.Types.Lens qualified as LSPL
import Language.LSP.VFS qualified as LSP
import Pedant.FileResolver qualified as Resolve
import Pedant.Parser qualified as Parser
import Pedant.TypeCheck qualified as TypeCheck
import Pedant.Types qualified as Types
import System.FilePath qualified as FilePath
import System.Log.Logger (debugM)
import Data.List.NonEmpty qualified as NonEmpty

handlers :: LSP.Handlers (LSP.LspM ())
handlers =
  mconcat
    [ LSP.notificationHandler LSP.SInitialized $ \_msg -> do
        liftIO $ debugM "reactor.handle" "Processing the Initialized notification"

        -- We're initialized! Lets send a showMessageRequest now
        LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "Initializing")

        -- We can dynamically register a capability once the user accepts it
        LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "Turning on code lenses dynamically")

        let regOpts = LSP.CodeLensRegistrationOptions Nothing Nothing (Just False)

        -- Register c+ode lens ability to read types
        void $
          LSP.registerCapability LSP.STextDocumentCodeLens regOpts $ \_req responder -> do
            liftIO $ debugM "reactor.handle" "Processing a textDocument/codeLens request"
            let doc = _req ^. LSPL.params . LSPL.textDocument . LSPL.uri
                filename = LSP.uriToFilePath doc
            case filename of
              Just f -> do
                types <- Reader.runReaderT (unwrapResolver $ getTypes f) (FilePath.dropFileName f)
                let rsp =
                      LSP.List
                        (map (\(line, message) -> LSP.CodeLens (LSP.mkRange (fromInteger (toInteger line)) 0 0 100) (Just (LSP.Command message "lsp-type" Nothing)) Nothing) (Map.toList types))
                responder (Right rsp)
              Nothing ->
                responder (Right $ LSP.List []),
      LSP.notificationHandler LSP.STextDocumentDidSave checkForErrors,
      LSP.notificationHandler LSP.STextDocumentDidChange checkForErrors,
      LSP.notificationHandler LSP.SWorkspaceDidChangeWatchedFiles (\_ -> pure ()),
      LSP.notificationHandler LSP.STextDocumentDidOpen checkForErrors
    ]

getUri :: (LSPL.HasParams s a1, LSPL.HasTextDocument a1 a2, LSPL.HasUri a2 a3) => s -> a3
getUri msg = msg ^. LSPL.params . LSPL.textDocument . LSPL.uri

newtype LspVFSResolver c m a = LspVFSResolver {unwrapResolver :: Reader.ReaderT String (LSP.LspT c m) a} deriving (Functor, Applicative, Monad)

instance Resolve.ModuleResolvingMonad (LspVFSResolver config IO) where
  readModule moduleName = LspVFSResolver $ do
    rootDirectory <- Reader.ask
    virtualFileContents <- LSP.getVirtualFile $ LSP.toNormalizedUri $ LSP.filePathToUri (rootDirectory FilePath.</> moduleName FilePath.<.> "ped")
    case virtualFileContents of
      Just vf -> return . Just $ LSP.virtualFileText vf
      Nothing -> return Nothing

checkForErrors :: (LSPL.HasParams s a1, LSPL.HasTextDocument a1 a2, LSPL.HasUri a2 LSP.Uri) => s -> LSP.LspT () IO ()
checkForErrors msg = do
  let uri = getUri msg
      fileName = LSP.uriToFilePath uri
  LSP.sendNotification LSP.SWindowShowMessage (LSP.ShowMessageParams LSP.MtInfo "file changed")
  liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName
  case LSP.uriToFilePath uri of
    Just fp -> do
      diagnostics <- Reader.runReaderT (unwrapResolver $ getErrors (FilePath.takeBaseName fp)) (FilePath.dropFileName fp)
      LSP.publishDiagnostics 100 (LSP.toNormalizedUri uri) (Just 0) (partitionBySource diagnostics)
    Nothing ->
      liftIO $ debugM "reactor.handle" $ "Processing DidSaveTextDocument  for: " ++ show fileName

runLSP :: IO Int
runLSP =
  LSP.runServer $
    LSP.ServerDefinition
      { LSP.onConfigurationChange = const $ const $ Right (),
        LSP.defaultConfig = (),
        LSP.doInitialize = \env _req -> pure $ Right env,
        LSP.staticHandlers = handlers,
        LSP.interpretHandler = \env -> LSP.Iso (LSP.runLspT env) liftIO,
        LSP.options = lspOptions
      }

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    (Just True)
    (Just LSP.TdSyncIncremental)
    (Just False)
    (Just False)
    (Just $ LSP.InR $ LSP.SaveOptions $ Just False)

lspOptions :: LSP.Options
lspOptions =
  LSP.defaultOptions
    { LSP.textDocumentSync = Just syncOptions,
      LSP.executeCommandCommands = Just ["lsp-ped-command"]
    }

generalError :: T.Text -> LSP.Diagnostic
generalError message = 
  LSP.Diagnostic
    (LSP.Range (LSP.Position 0 0) (LSP.Position 2 0))
    (Just LSP.DsError)
    Nothing -- code
    (Just "lsp-ped") -- source
    message
    Nothing -- tags
    (Just (LSP.List []))

-- Gets parse and type errors and provides them as LSP diagnostics
getErrors :: Resolve.ModuleResolvingMonad m => String -> m [LSP.Diagnostic]
getErrors moduleName = do
  resolveResult <- Resolve.resolve moduleName
  contents <- Resolve.readModule moduleName
  case resolveResult of
    Right modules ->
      case TypeCheck.typeCheck TypeCheck.emptyTypeCheckState modules of
        (Nothing, _) ->
          return []
        (Just err, _) ->
          case contents of
            Just c -> do
              let diag = Parser.makeErrorBundle err moduleName c
              return [parseErrorToDiagnostic diag]
            Nothing ->
              return [generalError "Error, Could not read file" ]
    Left err -> 
      return (map parseErrorToDiagnostic (NonEmpty.toList err))

--   in this file. It does this by creating a map of line numbers to error descriptions
getTypes :: Resolve.ModuleResolvingMonad m => String -> m (Map.Map Int T.Text)
getTypes name = do
  resolveResult <- Resolve.resolve name
  case resolveResult of
    Right modules ->
      case TypeCheck.typeCheck TypeCheck.emptyTypeCheckState modules of
        (_, state) ->
          let entries =
                map
                  (\(_, TypeCheck.VariableInfo scheme _ (Parser.Assignment _ _ expression)) -> (Parser.pdOffset . Parser.positionedData $ expression, Types.pPrint scheme))
                  (OMap.toAscList (TypeCheck.teVarMap $ TypeCheck.tcsEnv state))
           in return (Map.fromList entries)
    Left _ ->
      return Map.empty

toUInt :: Int -> LSP.UInt
toUInt = fromInteger . toInteger

parseErrorToDiagnostic :: Parser.PedantParseError -> LSP.Diagnostic
parseErrorToDiagnostic err =
  LSP.Diagnostic
    (LSP.Range (LSP.Position (toUInt $ Types.ppeRow err) (toUInt $ Types.ppeColumn err)) (LSP.Position (toUInt $ Types.ppeEndRow err) (toUInt $ Types.ppeEndColumn err)))
    (Just LSP.DsError)
    Nothing -- code
    (Just "lsp-ped") -- source
    (Types.ppeErrString err)
    Nothing -- tags
    (Just (LSP.List []))