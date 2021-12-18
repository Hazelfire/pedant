{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The parser for pedant, a small dimensional programming language
module Pedant where

import Control.Monad
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
import qualified Data.Text.IO as T
import qualified Language.LSP.Types as LSP
import Pedant.FileResolver (Module (..), resolve)
import qualified Pedant.LSP as LSP
import Pedant.TypeCheck
import Pedant.Types
import qualified System.Environment as Env
import Text.Megaparsec

pedantError :: Positioned TypeError -> String -> Text -> PedantParseError
pedantError te@(Positioned (PositionData offset l) _) name contents =
  let initialPosState =
        PosState
          { pstateInput = contents,
            pstateOffset = 0,
            pstateSourcePos = initialPos name,
            pstateTabWidth = defaultTabWidth,
            pstateLinePrefix = ""
          }
      ([(_, sourcePos), (_, endSourcePos)], _) = attachSourcePos id [offset, offset + l] initialPosState
      newPosState = initialPosState {pstateInput = contents, pstateOffset = 0}
      errorBundle = ParseErrorBundle (FancyError offset (Set.singleton (ErrorCustom te)) :| []) newPosState
   in PedantParseError
        { ppeErrString = T.pack $ showErrorComponent te,
          ppeColumn = unPos (sourceColumn sourcePos) - 1,
          ppeRow = unPos (sourceLine sourcePos) - 1,
          ppeEndColumn = unPos (sourceColumn endSourcePos) - 1,
          ppeEndRow = unPos (sourceLine endSourcePos) - 1,
          ppePrint = T.pack $ errorBundlePretty errorBundle
        }

getTypes :: String -> IO (Map.Map Int T.Text)
getTypes name = do
  case resolve name of
    Right modules ->
      case typeCheck emptyTypeCheckState parsed of
        (_, state) ->
          let offsetTypes =
                mapMaybe
                  ( \case
                      AssignmentStatement assignment ->
                        case assignmentExpression assignment of
                          Positioned (PositionData offset _) _ ->
                            case tcsEnv state of
                              TypeEnv varMap ->
                                case Map.lookup (assignmentName assignment) varMap of
                                  Just scheme ->
                                    Just (offset, pPrint scheme)
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
              (positions, _) = attachSourcePos id (map fst offsetTypes) initialPosState
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
    ("compile" : fileName : _) -> do
      contents <- T.pack <$> readFile fileName
      case parseProgram fileName contents of
        Right a -> do
          case typeCheck emptyTypeCheckState a of
            (Nothing, valid) -> do
              let program = List.reverse $ OMap.assocs (tcsExecutionExpressions valid)
               in case executeProgram OMap.empty program of
                    Right result -> do
                      forM_ (List.reverse (OMap.assocs result)) $ \(name, value) ->
                        T.putStrLn $ name <> " = " <> T.pack (show value) <> " " <> maybe "" (T.pack . show) (envLookup name (tcsEnv valid))
                    Left err ->
                      T.putStrLn err
            (Just err, _) -> do
              let diag = pedantError err fileName contents
              putStrLn (ppePrint diag)
        Left b -> putStrLn (ppePrint (NonEmpty.head b))
    _ -> putStrLn "pedant [file]"
  where
    envLookup :: T.Text -> TypeEnv -> Maybe Scheme
    envLookup key (TypeEnv d) = Map.lookup key d

executeProgram :: OMap.OMap T.Text NumericValue -> [(T.Text, ExecutionExpression)] -> Either T.Text (OMap.OMap T.Text NumericValue)
executeProgram values ((name, expr) : rest) =
  let value = evaluateExpression values expr
   in case value of
        Left err -> Left err
        Right result -> executeProgram ((name, result) |< values) rest
executeProgram values [] = Right values

evaluateExpression :: OMap.OMap T.Text NumericValue -> ExecutionExpression -> Either T.Text NumericValue
evaluateExpression variables expression =
  case expression of
    EBinOp "*" x y -> (*) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp "/" x y -> (/) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp "+" x y -> (+) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp "-" x y -> (-) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp "^" x y -> (**) <$> evaluateExpression variables x <*> evaluateExpression variables y
    EBinOp ":" x y -> do
      xTerm <- evaluateExpression variables x
      yTerm <- evaluateExpression variables y
      case yTerm of
        ListValue z -> return $ ListValue (xTerm : z)
        _ -> Left "Right of : must be list"
    EBinOp "" fExp parExp -> do
      func <- evaluateExpression variables fExp
      case func of
        FuncValue arg expr -> evaluateExpression variables (bindVariable arg parExp expr)
        _ -> Left $ "Cannot call constant " <> T.pack (show func)
    EAccess x name -> do
      evaluatedX <- evaluateExpression variables x
      case evaluatedX of
        DictValue entries ->
          case Map.lookup name entries of
            Just entry -> return entry
            _ -> Left $ "Cannot access " <> name
        _ -> Left $ "Cannot access " <> name <> " because not dictionary"
    EBinOp op _ _ -> Left $ "No such binary operation " <> op
    EVariable name ->
      case OMap.lookup name variables of
        Just value -> return value
        Nothing -> Left $ "Could not find variable " <> name
    EConstant (ExecutionValueNumber num) -> return (NumberValue num)
    EConstant (ExecutionValueFunc arg expr) -> return (FuncValue arg expr)
    EConstant (ExecutionValueDict entries) -> do
      evaluatedEntries <- forM (Map.toList entries) $ \(key, value) -> do
        evaluatedValue <- evaluateExpression variables value
        return (key, evaluatedValue)
      return (DictValue (Map.fromList evaluatedEntries))
    EConstant ExecutionValueEmptyList -> do
      return (ListValue [])
    ENegate expr -> negate <$> evaluateExpression variables expr

bindVariable :: T.Text -> ExecutionExpression -> ExecutionExpression -> ExecutionExpression
bindVariable name r (EVariable n)
  | name == n = r
  | otherwise = EVariable n
bindVariable name r (EBinOp op e1 e2) = EBinOp op (bindVariable name r e1) (bindVariable name r e2)
bindVariable name r (EAccess e1 x) = EAccess (bindVariable name r e1) x
bindVariable _ _ (EConstant v) = EConstant v
bindVariable name r (ENegate e1) = ENegate (bindVariable name r e1)
