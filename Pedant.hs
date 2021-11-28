{-# LANGUAGE OverloadedStrings #-}

-- | The parser for dimensional, a small dimensional programming language
module Test where

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
import LSP
import qualified Language.LSP.Types as LSP
import Parser
import qualified System.Environment as Env
import Text.Megaparsec
import TypeCheck
import Types

newtype SimpleError = SimpleError String
  deriving (Eq, Ord)

instance ShowErrorComponent SimpleError where
  showErrorComponent (SimpleError s) = s

pedantError :: TypeError -> String -> Text -> PedantParseError
pedantError (TypeError (PositionData offset) err) name contents =
  let initialPosState =
        PosState
          { pstateInput = contents,
            pstateOffset = 0,
            pstateSourcePos = initialPos name,
            pstateTabWidth = defaultTabWidth,
            pstateLinePrefix = ""
          }
      ([(a, sourcePos)], posState) = attachSourcePos id [offset] initialPosState
      newPosState = initialPosState {pstateSourcePos = sourcePos, pstateInput = contents, pstateOffset = 0}
      error = ParseErrorBundle (FancyError offset (Set.singleton (ErrorCustom (SimpleError err))) :| []) newPosState
   in PedantParseError
        { ppeErrString = err,
          ppeColumn = unPos $ sourceColumn sourcePos,
          ppeRow = unPos $ sourceLine sourcePos,
          ppePrint = errorBundlePretty error
        }

getErrors :: String -> IO [LSP.Diagnostic]
getErrors name = do
  contents <- T.pack <$> readFile name
  case parseProgram name contents of
    Right parsed ->
      case typeCheck emptyTypeCheckState parsed of
        Right valid ->
          return []
        Left err -> do
          let diag = pedantError err name contents
          return [parseErrorToDiagnostic diag]
    Left err ->
      return [parseErrorToDiagnostic (NonEmpty.head err)]

parseErrorToDiagnostic :: PedantParseError -> LSP.Diagnostic
parseErrorToDiagnostic err =
  LSP.Diagnostic
    (LSP.Range (LSP.Position (ppeColumn err) (ppeRow err)) (LSP.Position (ppeColumn err) (ppeRow err + 1)))
    (Just LSP.DsError)
    Nothing -- code
    (Just "lsp-ped") -- source
    (T.pack $ ppeErrString err)
    Nothing -- tags
    (Just (LSP.List []))

-- | Main Function
main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    ("lsp" : _) -> do
      void $ LSP.runLSP getErrors
    ("compile" : name : _) -> do
      contents <- T.pack <$> readFile name
      case parseProgram name contents of
        Right a -> do
          case typeCheck emptyTypeCheckState a of
            Right valid -> do
              let program = List.reverse $ OMap.assocs (fmap snd (tcsVariables valid))
               in case executeProgram OMap.empty program of
                    Right result -> do
                      forM_ (List.reverse (OMap.assocs result)) $ \(name, value) ->
                        putStrLn $ name ++ " = " ++ show value
                    Left err ->
                      putStrLn err
            Left err -> do
              let diag = pedantError err name contents
              putStrLn (ppePrint diag)
        Left b -> putStrLn (ppePrint (NonEmpty.head b))
    _ -> putStrLn "dimensional [file]"

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
    EBinOp App (EVariable name) x -> Left $ "No such function " ++ name
    EBinOp App (EConstant val) x -> Left $ "Cannot call constant " ++ show val
    EBinOp App (EBinOp op x1 x2) x -> Left $ "Cannot call operator " ++ show x1 ++ " " ++ show x2
    EBinOp App (ENegate val) x -> Left $ "Cannot call negation of " ++ show val
    EBinOp App (EAccess _ val) x -> Left $ "Cannot call access of " ++ show val
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
    EConstant (ExecutionValueDict entries) -> do
      evaluatedEntries <- forM (Map.toList entries) $ \(key, value) -> do
        evaluatedValue <- evaluateExpression variables value
        return (key, evaluatedValue)
      return (DictValue (Map.fromList evaluatedEntries))
    EConstant (ExecutionValueList list) -> do
      results <- mapM (evaluateExpression variables) list
      return (ListValue results)
    ENegate expr -> negate <$> evaluateExpression variables expr
