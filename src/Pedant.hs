{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | The parser for pedant, a small dimensional programming language
module Pedant (pedantMain, evaluatePedantFile, EvaluationResult (..)) where

import Control.Monad (forM, void)
import Data.Bifunctor qualified as Bifunctor
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Map.Ordered ((|<))
import Data.Map.Ordered qualified as OMap
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Pedant.InBuilt qualified as InBuilt
import Pedant.FileResolver qualified as Resolver
import Pedant.LSP qualified as LSP
import Pedant.Parser
  ( makeErrorBundle,
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
      ( ppePrint
      ),
    PrettyPrint (pPrint),
    Scheme,
    VariableName (VariableName),
  )
import System.Environment qualified as Env
import qualified Pedant.TypeCheck as TypeCheck


-- | Main Function
pedantMain :: IO ()
pedantMain = do
  args <- Env.getArgs
  case args of
    ("lsp" : _) -> do
      void LSP.runLSP
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
            case value of
              (FuncValue _ _) ->
                moduleName <> "." <> name <> " : " <> pPrint scheme
              (InternalFunctionValue _) ->
                moduleName <> "." <> name <> " : " <> pPrint scheme
              _ ->
                moduleName <> "." <> name <> " = " <> T.pack (show value) <> " " <> pPrint scheme
        )
        (List.reverse (OMap.assocs s))
  show (ParseError err) =
    T.unpack $ ppePrint err
  show (TypeCheckingError err) = T.unpack $ ppePrint err
  show (EvaluationError err) = T.unpack err

-- Takes a file and returns it's result from evaluation
evaluatePedantFile :: String -> IO EvaluationResult
evaluatePedantFile fileName = do
  resolveResult <- Resolver.resolveIO fileName
  contents <- T.readFile fileName
  -- After the file has been read and resolved
  case resolveResult of
    Right modules -> do
      -- Typecheck modules
      case typeCheck emptyTypeCheckState modules of
        (Nothing, valid) -> do
          let (TypeEnv env) = tcsEnv valid
          let program = List.reverse $ map (Bifunctor.second TypeCheck.variableInfoExecutionExpression) $ OMap.assocs env
           in -- Now execute program
              case executeProgram OMap.empty program of
                Right result ->
                  let valueTypeMap = OMap.intersectionWith (\ _ (TypeCheck.VariableInfo scheme _ _) value -> (value, scheme)) env result
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
