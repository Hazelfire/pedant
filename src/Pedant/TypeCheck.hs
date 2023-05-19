{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TypeChecker for Pedant.
module Pedant.TypeCheck
  ( TypeEnv (..),
    typeCheck,
    TypeError (..),
    emptyTypeCheckState,
    TypeCheckState (..),
    VariableName (..),
    VariableInfo(..)
  )
where

import Control.Monad.Except
import Control.Monad.State hiding (state)
import qualified Data.Map as Map
import qualified Data.Map.Ordered as OMap
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import Debug.Trace (trace, traceShowId)
import qualified Pedant.FileResolver as Resolver
import qualified Pedant.InBuilt as InBuilt
import qualified Pedant.Parser as Parser
import Pedant.Types
import qualified Text.Megaparsec as Megaparsec


-- | The state of the type checker
data TypeCheckState = TypeCheckState
  { -- | The environment of the checker. This contains references to all the variables and schemes of those variables currently declared.
    tcsEnv :: TypeEnv,
    -- | Substitutions, the current substitutions that are required for the expression to unify
    tcsSubs :: Substitution,
    -- | Units, the units currently declared
    tcsUnits :: Set.Set VariableName,
    -- | A list of the modules that have been checked
    tcsCheckedModules :: Set.Set T.Text,
    tcsCurrentModule :: T.Text
  }

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState = TypeCheckState (TypeEnv OMap.empty) nullSubst Set.empty Set.empty ""

nullSubst :: Substitution
nullSubst = Substitution Map.empty Map.empty

subUnion :: Substitution -> Substitution -> Substitution
subUnion a b =
  Substitution
    { subTypes = subTypes a `Map.union` subTypes b,
      subDimensions = subDimensions a `Map.union` subDimensions b
    }

composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = appliedSubs `subUnion` s1
  where
    appliedSubs =
      Substitution
        { subTypes = Map.map (apply s1) (subTypes s2),
          subDimensions = Map.map (apply s1) (subDimensions s2)
        }

data VariableInfo = VariableInfo {
  variableInfoScheme :: Scheme,
  variableInfoExecutionExpression :: ExecutionExpression,
  variableInfoParserStatement :: Parser.Assignment
} deriving (Show)

newtype TypeEnv = TypeEnv { teVarMap :: OMap.OMap VariableName VariableInfo }
  deriving (Show)


instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (map (variableInfoScheme . snd) $ OMap.assocs env)
  apply s (TypeEnv env) = TypeEnv (fmap (\vi -> vi { variableInfoScheme = apply s (variableInfoScheme vi) }) env)

instance Types TypeCheckState where
  ftv state = ftv (tcsEnv state)
  apply s state = state {tcsEnv = apply s (tcsEnv state)}


newtype TypeName = TypeName T.Text

type TI a = ExceptT (Parser.Positioned TypeError) (State TIState) a

runTI :: TI a -> (Either (Parser.Positioned TypeError) a, TIState)
runTI t =
  runState (runExceptT t) initTIState
  where
    initTIState = TIState {tiSupply = 0}

newTyVar :: TI Type
newTyVar = do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (PolyType ("a" <> T.pack (show (tiSupply s))))

newTyDimension :: T.Text -> TI Dimension
newTyDimension prefix =
  do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (NormDim $ Map.singleton (PolyDim $ prefix <> T.pack (show (tiSupply s))) 1)


liftUMtoTI :: Parser.PositionData -> ReasonForUnification -> UM a -> TI a
liftUMtoTI p reason m = do
  initState <- get
  case runState (runExceptT m) initState of
    (Right result, state) -> do
      put state
      return result
    (Left err, _) -> throwError $ Parser.Positioned p $ UnificationError reason err

wrapError :: Type -> Type -> UM Substitution -> UM Substitution
wrapError t1 t2 child = child `catchError` addUnificationLayer
  where
    addUnificationLayer :: UnificationTrace -> UM a
    addUnificationLayer errStack = throwError ((t1, t2) : errStack)

varBind :: T.Text -> Type -> UM Substitution
varBind u t
  | t == PolyType u = return nullSubst
  | u `Set.member` ftv t =
    throwError
      [(PolyType u, t)]
  | otherwise = return (nullSubst {subTypes = Map.singleton u t})

typeCheck :: TypeCheckState -> [Resolver.Module] -> (Maybe (Parser.Positioned TypeError), TypeCheckState)
typeCheck tcState (currentModule : rest) =
  trace ("Current module: " <> T.unpack (Resolver.moduleName currentModule)) $
    case typeCheckFile tcState currentModule of
      (Just err, newState) -> (Just err, newState)
      (Nothing, newState) -> do
        typeCheck newState rest
typeCheck tcState [] =
  (Nothing, tcState)


importModule :: T.Text -> Parser.Positioned T.Text -> [Parser.Positioned T.Text] -> TypeCheckState -> TI TypeCheckState
importModule moduleName (Parser.Positioned moduleNamePos importedModuleName) imports oldState =
    if importedModuleName `Set.member` tcsCheckedModules oldState
      then do
        let foldImports = foldM $ \currState (Parser.Positioned importNamePos importName) -> do
              let (TypeEnv env) = tcsEnv currState
              case OMap.lookup (VariableName importedModuleName importName) env of
                Just vi ->
                  -- The item imported is a variable. I simply write this down
                  -- as a variable declaration
                  let newTcState = addToEnv (VariableName moduleName importName) (vi { variableInfoExecutionExpression = EVariable (VariableName importedModuleName importName)}) (tcsEnv currState)
                   in return (currState {tcsEnv = newTcState})
                Nothing ->
                  -- Is it an imported unit?
                  if VariableName importedModuleName importName `Set.member` tcsUnits oldState
                    then
                      let newUnits = VariableName moduleName importName `Set.insert` tcsUnits currState
                       in return (currState {tcsUnits = newUnits})
                    else throwError $ Parser.Positioned importNamePos (MissingImportError importedModuleName importName)
        foldImports oldState imports
      else throwError $ Parser.Positioned moduleNamePos (MissingModuleError importedModuleName)

wrapFunctionArgs :: [T.Text] -> ExecutionExpression -> ExecutionExpression
wrapFunctionArgs (arg : rest) expr = EConstant (ExecutionValueFunc arg (wrapFunctionArgs rest expr))
wrapFunctionArgs [] expr = expr


foldSubst :: Traversable t => t Substitution -> Substitution
foldSubst = foldr composeSubst nullSubst
