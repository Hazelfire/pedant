{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Pedant.FileResolver (resolve, resolveIO, Module (..), ModuleResolvingMonad(..)) where

import qualified Control.Exception as Exception
import Control.Monad (forM_)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pedant.Parser as Parser
import Pedant.Types (PedantParseError (..))
import qualified System.FilePath as FilePath
import Control.Exception (IOException)

-- | A module. A collection of files before it is run through the type checker
data Module = Module
  { moduleName :: T.Text,
    moduleStatements :: [Parser.Statement]
  }
  deriving (Show)

data ResolverState = ResolverState
  { moduleCache :: Map.Map T.Text Module,
    moduleStack :: [Module],
    moduleReturn :: [Module]
  }


class Monad m => ModuleResolvingMonad m where
  readModule :: String -> m (Maybe T.Text)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right b) = Just b
rightToMaybe (Left _) = Nothing

instance ModuleResolvingMonad (Reader.ReaderT String IO) where
  readModule fileName = do
    rootDirectory <- Reader.ask
    rightToMaybe <$> IO.liftIO (Exception.try (T.readFile (rootDirectory FilePath.</> moduleNameToPath fileName)) :: IO (Either IOException T.Text))

moduleNameToPath :: String -> String
moduleNameToPath path = map repl path ++ ".ped"
  where
    repl :: Char -> Char
    repl '.' = FilePath.pathSeparator
    repl x = x

emptyResolverState :: ResolverState
emptyResolverState = ResolverState Map.empty [] []

type ResolverMonad m a = Except.ExceptT (NonEmpty PedantParseError) (State.StateT ResolverState m) a

resolveIO :: String -> IO (Either (NonEmpty PedantParseError) [Module])
resolveIO startFileName = 
  let rootDirectory = FilePath.dropFileName startFileName
      startingModule = FilePath.takeBaseName startFileName
  in
    Reader.runReaderT (resolve startingModule) rootDirectory

resolve :: ModuleResolvingMonad m => String -> m (Either (NonEmpty PedantParseError) [Module])
resolve startingModule =
  fst <$> State.runStateT (Except.runExceptT (resolve' startingModule)) emptyResolverState

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

errorReadingModule :: T.Text -> PedantParseError
errorReadingModule m =
  let errorMessage =
        T.concat
          [ "Could not read from module ",
            m
          ]
   in PedantParseError errorMessage 1 1 2 1 errorMessage


resolve' :: ModuleResolvingMonad m => String -> ResolverMonad m [Module]
resolve' startingModule = do
  readResult <- (Reader.lift . Reader.lift . readModule) startingModule
  case readResult of
    Nothing -> Except.throwError $ errorReadingModule (T.pack startingModule) :| []
    Just contents ->
      case Parser.parseProgram startingModule contents of
        Left err -> Except.throwError err
        Right statements -> do
          let newModule = Module (T.pack startingModule) statements
              importedModules = Maybe.mapMaybe getImportName statements
          State.modify
            ( \s ->
                ResolverState
                  { moduleStack = newModule : moduleStack s,
                    moduleCache = Map.insert (T.pack startingModule) newModule (moduleCache s),
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
                          T.pack startingModule,
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
                    Reader.void (resolve' (T.unpack m))
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
