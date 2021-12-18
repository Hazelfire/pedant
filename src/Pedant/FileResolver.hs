{-# LANGUAGE OverloadedStrings #-}

module Pedant.FileResolver (resolve, Module (..)) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State as State
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pedant.Parser as Parser
import qualified Data.Map.Ordered as OMap
import Pedant.Types (PedantParseError (..))

data Module = Module
  { moduleName :: T.Text,
    moduleStatements :: OMap.OMap T.Text Parser.Statement
  }

data ResolverState = ResolverState
  { moduleCache :: Map.Map T.Text Module,
    moduleStack :: [Module],
    moduleReturn :: [Module]
  }

emptyResolverState :: ResolverState
emptyResolverState = ResolverState Map.empty [] []

type ResolverMonad a = Except.ExceptT (NonEmpty PedantParseError) (State.StateT ResolverState IO) a

resolve :: String -> IO (Either (NonEmpty PedantParseError) (OMap.OMap T.Text Module))
resolve startFileName = do
  fst <$> State.runStateT (Except.runExceptT (resolve' startFileName)) emptyResolverState

tailOn :: Eq a => a -> [a] -> [a]
tailOn _ [] = []
tailOn x (h : rest)
  | x == h = h : rest
  | otherwise = tailOn x rest

errorReadingModule :: Exception.IOException -> T.Text -> PedantParseError
errorReadingModule _ m =
  let errorMessage =
        T.concat
          [ "Could not read from module ",
            m
          ]
   in PedantParseError errorMessage 1 1 2 1 errorMessage

statementName :: Parser.Statement -> [(String, Importable)]
statementName (AssignmentStatement assignment) = Set.insertParser.assignmentName assignment
statementName (Parser.UnitStatement units) = 

data Importable = ImportableVariable Parser.AssignmentStatement
                | ImportableUnit T.Text

resolve' :: String -> ResolverMonad (OMap.OMap T.Text Module)
resolve' fileName = do
  readResult <- IO.liftIO $ Exception.try $ T.readFile (fileName ++ ".ped")
  case readResult of
    Left err -> Except.throwError $ errorReadingModule err (T.pack fileName) :| []
    Right contents ->
      case Parser.parseProgram fileName contents of
        Left err -> Except.throwError err
        Right statements -> do
          let newModule = Module (T.pack fileName) (OMap.fromList ( map (\x -> (moduleName x, x)) statements))
              importedModules = Maybe.mapMaybe getImportName statements
          State.modify
            ( \s ->
                ResolverState
                  { moduleStack = newModule : moduleStack s,
                    moduleCache = Map.insert (T.pack fileName) newModule (moduleCache s),
                    moduleReturn = newModule : moduleStack s
                  }
            )
          forM_ importedModules $ \m -> do
            stack <- State.gets moduleStack
            let parentModules = map moduleName stack
            if m `elem` parentModules
              then
                let errorMessage =
                      T.concat
                        [ "Module ",
                          T.pack fileName,
                          " can not import module ",
                          m,
                          " as doing so would create a cycle, through ",
                          T.intercalate " -> " (tailOn m parentModules)
                        ]
                 in Except.throwError $ PedantParseError errorMessage 1 1 2 1 errorMessage :| []
              else do
                cache <- State.gets moduleCache
                case Map.lookup m cache of
                  Just _ ->
                    -- Already loaded this module, skip
                    pure ()
                  Nothing ->
                    -- We haven't loaded this module yet, resolve it
                    () <$ resolve' (T.unpack m)
          State.modify
            ( \s ->
                s
                  { moduleStack = tail (moduleStack s)
                  }
            )
          State.gets moduleReturn

getImportName :: Parser.Statement -> Maybe T.Text
getImportName (Parser.ImportStatement name _) = Just name
getImportName _ = Nothing
