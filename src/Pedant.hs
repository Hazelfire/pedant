{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The parser for pedant, a small dimensional programming language
module Pedant (pedantMain, evaluatePedantFile, EvaluationResult (..)) where

import Control.Monad (forM, void)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map.Ordered ((|<))
import qualified Data.Map.Ordered as OMap
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.LSP.Types as LSP
import qualified Pedant.FileResolver as Resolve
import qualified Pedant.InBuilt as InBuilt
import qualified Pedant.LSP as LSP
import Pedant.Parser
  ( Assignment (assignmentExpression, assignmentName),
    PositionData (PositionData),
    Positioned (Positioned),
    Statement (AssignmentStatement),
    makeErrorBundle,
  )
import Pedant.TypeCheck
  ( TypeCheckState (tcsEnv),
    TypeEnv (TypeEnv),
    emptyTypeCheckState,
    typeCheck,
  )
import Pedant.Types
  ( ExecutionExpression (..),
    ExecutionValue
      ( ExecutionValueDict,
        ExecutionValueEmptyList,
        ExecutionValueFunc,
        ExecutionValueNumber
      ),
    InternalFunction (..),
    NumericValue (..),
    PedantParseError
      ( ppeColumn,
        ppeEndColumn,
        ppeEndRow,
        ppeErrString,
        ppePrint,
        ppeRow
      ),
    PrettyPrint (pPrint),
    Scheme,
    VariableName (VariableName),
  )
import qualified System.Environment as Env
import Text.Megaparsec
  ( PosState
      ( PosState,
        pstateInput,
        pstateLinePrefix,
        pstateOffset,
        pstateSourcePos,
        pstateTabWidth
      ),
    SourcePos (sourceLine),
    attachSourcePos,
    defaultTabWidth,
    initialPos,
    unPos,
  )

-- | getTypes, given a file, attempts to find the types of all assignments
--   in this file. It does this by creating a map of line numbers to error descriptions
getTypes :: String -> IO (Map.Map Int T.Text)
getTypes name = do
  resolveResult <- Resolve.resolve name
  contents <- readFile name
  case resolveResult of
    Right modules ->
      case typeCheck emptyTypeCheckState modules of
        (_, state) ->
          let matchingModules = filter ((== name) . T.unpack . Resolve.moduleName) modules
           in case matchingModules of
                [] ->
                  return Map.empty
                (myModule : _) ->
                  let offsetTypes =
                        mapMaybe
                          ( \case
                              AssignmentStatement assignment ->
                                let (Positioned (PositionData offset _) _) = assignmentExpression assignment
                                    (TypeEnv varMap) = tcsEnv state
                                 in case OMap.lookup (VariableName (T.pack name) (assignmentName assignment)) varMap of
                                      Just (scheme, _) ->
                                        Just (offset, pPrint scheme)
                                      _ -> Nothing
                              _ -> Nothing
                          )
                          (Resolve.moduleStatements myModule)
                      initialPosState =
                        PosState
                          { pstateInput = contents,
                            pstateOffset = 0,
                            pstateSourcePos = initialPos name,
                            pstateTabWidth = defaultTabWidth,
                            pstateLinePrefix = ""
                          }
                      (positions, _) = attachSourcePos id (map fst offsetTypes) initialPosState
                      entries = zip (map ((\a -> a - 1) . unPos . sourceLine . snd) positions) (map snd offsetTypes)
                   in return (Map.fromList entries)
    Left _ ->
      return Map.empty

getErrors :: String -> IO [LSP.Diagnostic]
getErrors name = do
  resolveResult <- Resolve.resolve name
  contents <- T.readFile name
  case resolveResult of
    Right modules ->
      case typeCheck emptyTypeCheckState modules of
        (Nothing, _) ->
          return []
        (Just err, _) -> do
          let diag = makeErrorBundle err name contents
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
    (ppeErrString err)
    Nothing -- tags
    (Just (LSP.List []))

-- | Main Function
pedantMain :: IO ()
pedantMain = do
  args <- Env.getArgs
  case args of
    ("lsp" : _) -> do
      void $ LSP.runLSP getErrors getTypes
    ("compile" : fileName : _) -> print =<< evaluatePedantFile fileName
    _ -> putStrLn "pedant compile [file]"

data EvaluationResult
  = EvaluationSuccess VariableValues
  | ParseError PedantParseError
  | TypeCheckingError PedantParseError
  | EvaluationError T.Text

type VariableValues = OMap.OMap VariableName (NumericValue, Scheme)

instance Show EvaluationResult where
  show (EvaluationSuccess s) =
    T.unpack . T.unlines $
      map
        ( \(VariableName moduleName name, (value, scheme)) ->
            moduleName <> "." <> name <> " = " <> T.pack (show value) <> " " <> pPrint scheme
        )
        (List.reverse (OMap.assocs s))
  show (ParseError err) =
    T.unpack $ ppePrint err
  show (TypeCheckingError err) = T.unpack $ ppePrint err
  show (EvaluationError err) = T.unpack err

evaluatePedantFile :: String -> IO EvaluationResult
evaluatePedantFile fileName = do
  resolveResult <- Resolve.resolve fileName
  contents <- T.readFile fileName
  case resolveResult of
    Right modules -> do
      case typeCheck emptyTypeCheckState modules of
        (Nothing, valid) -> do
          let (TypeEnv env) = tcsEnv valid
          let program = List.reverse $ map (Bifunctor.second snd) $ OMap.assocs env
           in case executeProgram OMap.empty program of
                Right result ->
                  let valueTypeMap = OMap.intersectionWith (\_ (scheme, _) value -> (value, scheme)) env result
                   in return (EvaluationSuccess valueTypeMap)
                Left err ->
                  return (EvaluationError err)
        (Just err, _) -> do
          let diag = makeErrorBundle err fileName contents
          return (TypeCheckingError diag)
    Left b -> return (ParseError $ NonEmpty.head b)

executeProgram :: OMap.OMap VariableName NumericValue -> [(VariableName, ExecutionExpression)] -> Either T.Text (OMap.OMap VariableName NumericValue)
executeProgram values ((name, expr) : rest) =
  let value = evaluateExpression values expr
   in case value of
        Left err -> Left err
        Right result -> executeProgram ((name, result) |< values) rest
executeProgram values [] = Right values

evaluateExpression :: OMap.OMap VariableName NumericValue -> ExecutionExpression -> Either T.Text NumericValue
evaluateExpression variables expression =
  case expression of
    EBinOp "" fExp parExp -> do
      func <- evaluateExpression variables fExp
      case func of
        FuncValue arg expr -> evaluateExpression variables (bindVariable arg parExp expr)
        InternalFunctionValue (InternalFunction f) -> f <$> evaluateExpression variables parExp
        _ -> Left $ "Cannot call constant " <> T.pack (show func)
    EBinOp op x y ->
      let matchingOperations = filter ((== op) . InBuilt.opName) InBuilt.inBuiltBinaryOperations
       in case matchingOperations of
            [] -> Left $ "No such binary operation " <> op
            opDetails : _ ->
              case InBuilt.opFunc opDetails of
                InBuilt.BinFunc f -> f <$> evaluateExpression variables x <*> evaluateExpression variables y
                _ -> Left $ "Binary function not found " <> op
    EAccess x name -> do
      evaluatedX <- evaluateExpression variables x
      case evaluatedX of
        DictValue entries ->
          case Map.lookup name entries of
            Just entry -> return entry
            _ -> Left $ "Cannot access " <> name
        _ -> Left $ "Cannot access " <> name <> " because not dictionary"
    EVariable name ->
      case OMap.lookup name variables of
        Just value -> return value
        Nothing ->
          let (VariableName _ newName) = name
           in Left $ "Could not find variable " <> newName
    EConstant (ExecutionValueNumber num) -> return (NumberValue num)
    EConstant (ExecutionValueFunc arg expr) -> return (FuncValue arg expr)
    EConstant (ExecutionValueDict entries) -> do
      evaluatedEntries <- forM (Map.toList entries) $ \(key, value) -> do
        evaluatedValue <- evaluateExpression variables value
        return (key, evaluatedValue)
      return (DictValue (Map.fromList evaluatedEntries))
    EConstant ExecutionValueEmptyList -> do
      return (ListValue [])
    EInternalFunc f -> return $ InternalFunctionValue f
    ENegate expr -> negate <$> evaluateExpression variables expr

bindVariable :: T.Text -> ExecutionExpression -> ExecutionExpression -> ExecutionExpression
bindVariable name r (EVariable (VariableName moduleName n))
  | name == n = r
  | otherwise = EVariable (VariableName moduleName n)
bindVariable name r (EBinOp op e1 e2) = EBinOp op (bindVariable name r e1) (bindVariable name r e2)
bindVariable name r (EAccess e1 x) = EAccess (bindVariable name r e1) x
bindVariable _ _ (EConstant v) = EConstant v
bindVariable name r (ENegate e1) = ENegate (bindVariable name r e1)
bindVariable _ _ (EInternalFunc e1) = EInternalFunc e1
