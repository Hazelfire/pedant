{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The parser for pedant, a small dimensional programming language
module Pedant where

import Control.Monad
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map.Ordered ((|<))
import qualified Data.Map.Ordered as OMap
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import LSP
import qualified Language.LSP.Types as LSP
import Parser
import qualified System.Environment as Env
import Text.Megaparsec
import TypeCheck
import Types

instance ShowErrorComponent TypeError where
  showErrorComponent (TypeError _ s) = s
  errorComponentLen (TypeError (PositionData _ length) _) = length

pedantError :: TypeError -> String -> Text -> PedantParseError
pedantError te@(TypeError (PositionData offset length) err) name contents =
  let initialPosState =
        PosState
          { pstateInput = contents,
            pstateOffset = 0,
            pstateSourcePos = initialPos name,
            pstateTabWidth = defaultTabWidth,
            pstateLinePrefix = ""
          }
      ([(a, sourcePos), (_, endSourcePos)], posState) = attachSourcePos id [offset, offset + length] initialPosState
      newPosState = initialPosState {pstateInput = contents, pstateOffset = 0}
      error = ParseErrorBundle (FancyError offset (Set.singleton (ErrorCustom te)) :| []) newPosState
   in PedantParseError
        { ppeErrString = err,
          ppeColumn = unPos (sourceColumn sourcePos) - 1,
          ppeRow = unPos (sourceLine sourcePos) - 1,
          ppeEndColumn = unPos (sourceColumn endSourcePos) - 1,
          ppeEndRow = unPos (sourceLine endSourcePos) - 1,
          ppePrint = errorBundlePretty error
        }

getTypes :: String -> IO (Map.Map Int String)
getTypes name = do
  contents <- T.pack <$> readFile name
  case parseProgram name contents of
    Right parsed ->
      case typeCheck emptyTypeCheckState parsed of
        (_, state) ->
          let offsetTypes =
                mapMaybe
                  ( \case
                      AssignmentStatement assignment ->
                        case assignmentExpression assignment of
                          Positioned (PositionData offset length) _ ->
                            case tcsEnv state of
                              TypeEnv map ->
                                case Map.lookup (assignmentName assignment) map of
                                  Just scheme ->
                                    Just (offset, prettyPrintScheme scheme)
                                  _ -> Nothing
                      _ -> Nothing
                  )
                  parsed
              initialPosState =
                PosState
                  { pstateInput = contents,
                    pstateOffset = 0,
                    pstateSourcePos = initialPos name,
                    pstateTabWidth = defaultTabWidth,
                    pstateLinePrefix = ""
                  }
              (positions, posState) = attachSourcePos id (map fst offsetTypes) initialPosState
              entries = zip (map ((\a -> a - 1) . unPos . sourceLine . snd) positions) (map snd offsetTypes)
           in return (Map.fromList entries)
    Left _ ->
      return Map.empty

getErrors :: String -> IO [LSP.Diagnostic]
getErrors name = do
  contents <- T.pack <$> readFile name
  case parseProgram name contents of
    Right parsed ->
      case typeCheck emptyTypeCheckState parsed of
        (Nothing, _) ->
          return []
        (Just err, _) -> do
          let diag = pedantError err name contents
          return [parseErrorToDiagnostic diag]
    Left err ->
      return [parseErrorToDiagnostic (NonEmpty.head err)]

parseErrorToDiagnostic :: PedantParseError -> LSP.Diagnostic
parseErrorToDiagnostic err =
  LSP.Diagnostic
    (LSP.Range (LSP.Position (ppeRow err) (ppeColumn err)) (LSP.Position (ppeEndRow err) (ppeEndColumn err)))
    (Just LSP.DsError)
    Nothing -- code
    (Just "lsp-ped") -- source
    (T.pack $ ppeErrString err)
    Nothing -- tags
    (Just (LSP.List []))

-- | Main Function
pedantMain :: IO ()
pedantMain = do
  args <- Env.getArgs
  case args of
    ("lsp" : _) -> do
      void $ LSP.runLSP getErrors getTypes
    ("compile" : name : _) -> do
      contents <- T.pack <$> readFile name
      case parseProgram name contents of
        Right a -> do
          case typeCheck emptyTypeCheckState a of
            (Nothing, valid) -> do
              let program = List.reverse $ OMap.assocs (tcsExecutionExpressions valid)
               in case executeProgram OMap.empty program of
                    Right result -> do
                      forM_ (List.reverse (OMap.assocs result)) $ \(name, value) ->
                        putStrLn $ name ++ " = " ++ show value ++ " " ++ maybe "" show (envLookup name (tcsEnv valid))
                    Left err ->
                      putStrLn err
            (Just err, _) -> do
              let diag = pedantError err name contents
              putStrLn (ppePrint diag)
        Left b -> putStrLn (ppePrint (NonEmpty.head b))
    _ -> putStrLn "pedant [file]"
  where
    envLookup :: String -> TypeEnv -> Maybe Scheme
    envLookup key (TypeEnv d) = Map.lookup key d

executeProgram :: OMap.OMap String NumericValue -> [(String, ExecutionExpression)] -> Either String (OMap.OMap String NumericValue)
executeProgram values ((name, expr) : rest) =
  let value = evaluateExpression values expr
   in case value of
        Left err -> Left err
        Right result -> executeProgram ((name, result) |< values) rest
executeProgram values [] = Right values

evaluateExpression :: OMap.OMap String NumericValue -> ExecutionExpression -> Either String NumericValue
evaluateExpression variables expression =
  case expression of
    EBinOp Mult x y -> (*) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp Div x y -> (/) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp Add x y -> (+) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp Sub x y -> (-) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp Power x y -> (**) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp App (EVariable "ln") x -> log <$> evaluateExpression variables x
    EBinOp App fExp parExp -> do
      func <- evaluateExpression variables fExp
      case func of
        FuncValue arg exp -> evaluateExpression variables (bindVariable arg parExp exp)
        _ -> Left $ "Cannot call constant " ++ show func
    EAccess x name -> do
      evaluatedX <- evaluateExpression variables x
      case evaluatedX of
        DictValue entries ->
          case Map.lookup name entries of
            Just entry -> return entry
            _ -> Left $ "Cannot access " ++ name
        _ -> Left $ "Cannot access " ++ name ++ " because not dictionary"
    EVariable name ->
      case OMap.lookup name variables of
        Just value -> return value
        Nothing -> Left $ "Could not find variable " ++ name
    EConstant (ExecutionValueNumber num) -> return (NumberValue num)
    EConstant (ExecutionValueFunc arg exp) -> return (FuncValue arg exp)
    EConstant (ExecutionValueDict entries) -> do
      evaluatedEntries <- forM (Map.toList entries) $ \(key, value) -> do
        evaluatedValue <- evaluateExpression variables value
        return (key, evaluatedValue)
      return (DictValue (Map.fromList evaluatedEntries))
    EConstant (ExecutionValueList list) -> do
      results <- mapM (evaluateExpression variables) list
      return (ListValue results)
    ENegate expr -> negate <$> evaluateExpression variables expr

bindVariable :: String -> ExecutionExpression -> ExecutionExpression -> ExecutionExpression
bindVariable name r (EVariable n)
  | name == n = r
  | otherwise = EVariable n
bindVariable name r (EBinOp op e1 e2) = EBinOp op (bindVariable name r e1) (bindVariable name r e2)
bindVariable name r (EAccess e1 x) = EAccess (bindVariable name r e1) x
bindVariable name r (EConstant v) = EConstant v
bindVariable name r (ENegate e1) = ENegate (bindVariable name r e1)
