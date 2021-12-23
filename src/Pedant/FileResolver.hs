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
import Pedant.Types (PedantParseError (..))
import qualified System.FilePath as FilePath

-- | A module. A collection of files before it is run through the type checker
data Module = Module
  { moduleName :: T.Text,
    moduleStatements :: [Parser.Statement]
  }
  deriving (Show)

data ResolverState = ResolverState
  { moduleCache :: Map.Map T.Text Module,
    moduleStack :: [Module],
    moduleReturn :: [Module],
    moduleRootDirectory :: String
  }

emptyResolverState :: String -> ResolverState
emptyResolverState = ResolverState Map.empty [] []

type ResolverMonad a = Except.ExceptT (NonEmpty PedantParseError) (State.StateT ResolverState IO) a

resolve :: String -> IO (Either (NonEmpty PedantParseError) [Module])
resolve startFileName = do
  let rootDirectory = FilePath.dropFileName startFileName
  fst <$> State.runStateT (Except.runExceptT (resolve' (FilePath.takeBaseName startFileName))) (emptyResolverState rootDirectory)

-- | Finds the list of elements that come at and after the one specified
--
-- >>> tailOn 3 [1, 2, 3, 4, 5]
-- [3,4,5]
--
-- >>> tailOn 7 [1, 2, 3, 4, 5]
-- []
--
-- >>> tailOn 1 [1, 2, 3, 4, 5]
-- [1,2,3,4,5]
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

moduleNameToPath :: String -> String
moduleNameToPath path = map repl path ++ ".ped"
  where
    repl :: Char -> Char
    repl '.' = FilePath.pathSeparator
    repl x = x

resolve' :: String -> ResolverMonad [Module]
resolve' fileName = do
  rootDirectory <- State.gets moduleRootDirectory
  readResult <- IO.liftIO $ Exception.try (T.readFile (rootDirectory FilePath.</> moduleNameToPath fileName))
  case readResult of
    Left err -> Except.throwError $ errorReadingModule err (T.pack fileName) :| []
    Right contents ->
      case Parser.parseProgram fileName contents of
        Left err -> Except.throwError err
        Right statements -> do
          let newModule = Module (T.pack fileName) statements
              importedModules = Maybe.mapMaybe getImportName statements
          State.modify
            ( \s ->
                ResolverState
                  { moduleStack = newModule : moduleStack s,
                    moduleCache = Map.insert (T.pack fileName) newModule (moduleCache s),
                    moduleReturn = newModule : moduleStack s,
                    moduleRootDirectory = rootDirectory
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
getImportName (Parser.ImportStatement (Parser.Positioned _ name) _) = Just name
getImportName _ = Nothing
