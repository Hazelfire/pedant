{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TypeChecker for Pedant.
module Pedant.TypeCheck
  ( TypeEnv (..),
    typeCheck,
    TypeError (..),
    emptyTypeCheckState,
    TypeCheckState (..),
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
import qualified Pedant.InBuilt as InBuilt
import Pedant.Parser
import Pedant.Types
import qualified Text.Megaparsec as Megaparsec

-- | A Type Error. Contains the position where error was found and what
--   the problem is
data TypeError
  = UnificationError ReasonForUnification UnificationTrace
  | MissingUnitError T.Text
  | MissingVariableError T.Text
  | InternalError T.Text
  deriving (Eq)

data ReasonForUnification
  = BinaryOpUnificationReason T.Text (Positioned ParseExpression, Type) (Positioned ParseExpression, Type)
  | PrefixOpUnificationReason T.Text (Positioned ParseExpression, Type)
  | AccessUnificationReason (Positioned ParseExpression, Type) T.Text
  deriving (Eq)

instance Ord (Positioned TypeError) where
  compare (Positioned (PositionData a _) _) (Positioned (PositionData b _) _) = compare a b

type UnificationTrace = [(Type, Type)]

instance Megaparsec.ShowErrorComponent (Positioned TypeError) where
  showErrorComponent (Positioned _ (UnificationError reason _)) =
    case reason of
      BinaryOpUnificationReason "+" (p1, t1) (p2, t2) ->
        T.unpack $
          T.concat
            [ "Can only add dimension that are the same.\n",
              pPrint p1,
              " has the type ",
              pPrint t1,
              " and ",
              pPrint p2,
              " has the type ",
              pPrint t2
            ]
      BinaryOpUnificationReason "-" (p1, t1) (p2, t2) ->
        T.unpack $
          T.concat
            [ "Can only subtract dimensions that are the same.\n",
              pPrint p1,
              " has the type ",
              pPrint t1,
              " and ",
              pPrint p2,
              " has the type ",
              pPrint t2
            ]
      BinaryOpUnificationReason op (p1, t1) (p2, t2) ->
        T.unpack $
          T.concat
            [ op,
              " must be called on a number.\n",
              pPrint p1,
              " has the type ",
              pPrint t1,
              " and ",
              pPrint p2,
              " has the type ",
              pPrint t2
            ]
      PrefixOpUnificationReason op (p1, t1) ->
        T.unpack $
          T.concat
            [ op,
              " must be called on a number.\n",
              pPrint p1,
              " has the type ",
              pPrint t1
            ]
      AccessUnificationReason (p1, t1) key ->
        T.unpack $
          T.concat
            [ pPrint p1,
              " has type ",
              pPrint t1,
              " does not have the key ",
              key
            ]
  showErrorComponent (Positioned _ (MissingUnitError unitName)) =
    concat
      [ "unit ",
        T.unpack unitName,
        " not declared. Try adding a \"unit ",
        T.unpack unitName,
        "\" statement before this line"
      ]
  showErrorComponent (Positioned _ (MissingVariableError varName)) =
    concat
      [ "variable ",
        T.unpack varName,
        " not declared."
      ]
  showErrorComponent (Positioned _ (InternalError err)) =
    "INTERNAL ERROR. YOU SHOULD NOT BE GETTING THIS: "
      ++ T.unpack err
  errorComponentLen (Positioned (PositionData _ l) _) = l

data TypeCheckState = TypeCheckState
  { tcsEnv :: TypeEnv,
    tcsSubs :: Substitution,
    tcsUnits :: Set.Set T.Text,
    tcsExecutionExpressions :: OMap.OMap T.Text ExecutionExpression
  }

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState = TypeCheckState (TypeEnv Map.empty) nullSubst Set.empty OMap.empty

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

newtype TypeEnv = TypeEnv (Map.Map T.Text Scheme)
  deriving (Show)

remove :: T.Text -> TypeEnv -> TypeEnv
remove var (TypeEnv env) = TypeEnv (Map.delete var env)

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

instance Types TypeCheckState where
  ftv state = ftv (tcsEnv state)
  apply s state = state {tcsEnv = apply s (tcsEnv state)}

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where
    vars = Set.toList (ftv t `Set.difference` ftv env)

newtype TIState = TIState {tiSupply :: Int}

type TI a = ExceptT (Positioned TypeError) (State TIState) a

runTI :: TI a -> (Either (Positioned TypeError) a, TIState)
runTI t =
  runState (runExceptT t) initTIState
  where
    initTIState = TIState {tiSupply = 0}

newTyVar :: T.Text -> TI Type
newTyVar prefix =
  do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (PolyType (prefix <> T.pack (show (tiSupply s))))

newTyDimension :: T.Text -> TI Dimension
newTyDimension prefix =
  do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (NormDim $ Map.singleton (PolyDim $ prefix <> T.pack (show (tiSupply s))) 1)

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  nvars <- mapM (\_ -> newTyVar "a") vars
  ndims <- mapM (\_ -> newTyDimension "a") vars
  let s =
        nullSubst
          { subTypes = Map.fromList (zip vars nvars),
            subDimensions = Map.fromList (zip vars ndims)
          }
  return $ apply s t

-- | Unification Monad
type UM a = ExceptT UnificationTrace (State TIState) a

liftUMtoTI :: PositionData -> ReasonForUnification -> UM a -> TI a
liftUMtoTI p reason m = do
  initState <- get
  case runState (runExceptT m) initState of
    (Right result, state) -> do
      put state
      return result
    (Left err, _) -> throwError $ Positioned p $ UnificationError reason err

-- | Attempts to find a unification between dimensions
mguDim :: Dimension -> Dimension -> UM Substitution
mguDim (NormDim t) (NormDim u) =
  if u == t
    then return nullSubst
    else -- unifying dimensions is a bit tricky, and this method is not perfect and leaves out some possible (but rare) unifications

      let dividedOut = Map.filter (/= 0) $ Map.unionWith (+) t (Map.map negate u)
          polyDim =
            Maybe.mapMaybe
              ( \(k, v) ->
                  case (k, v) of
                    (PolyDim d, 1) -> Just (d, 1)
                    (PolyDim d, -1) -> Just (d, -1)
                    _ -> Nothing
              )
              (Map.toList dividedOut)
       in case polyDim of
            (firstDim, power) : _ ->
              let withoutPolyVar = Map.delete (PolyDim firstDim) dividedOut
                  dividedByPower = Map.map (`quot` (- power)) withoutPolyVar
               in return $ nullSubst {subDimensions = Map.singleton firstDim (NormDim dividedByPower)}
            [] ->
              throwError [(BaseDim (NormDim t), BaseDim (NormDim u))]
mguDim (PowDim t) (PowDim u) =
  trace (show (PowDim t) ++ " unify " ++ show (PowDim u)) $
    if u == t
      then return nullSubst
      else
        let dividedOut = Map.filter (/= 0) $ Map.unionWith (+) t (Map.map negate u)
            polyDim =
              Maybe.mapMaybe
                ( \(k, v) ->
                    case (k, v) of
                      (PolyDim d, 1) -> Just (d, 1)
                      (PolyDim d, -1) -> Just (d, -1)
                      _ -> Nothing
                )
                (Map.toList (traceShowId dividedOut))
         in case traceShowId polyDim of
              (firstDim, power) : _ ->
                let withoutPolyVar = Map.delete (PolyDim firstDim) dividedOut
                    dividedByPower = Map.map (`quot` (- power)) (traceShowId withoutPolyVar)
                 in return $ nullSubst {subDimensions = Map.singleton firstDim (PowDim (traceShowId dividedByPower))}
              [] ->
                throwError [(BaseDim (PowDim t), BaseDim (PowDim u))]
mguDim (NormDim u) (PowDim t) = do
  -- I can only unify BaseDims and PowDims if they both unify to dimensionless
  trace (show (NormDim u) ++ " unify " ++ show (PowDim t)) $
    ( do
        s1 <- mguDim (NormDim Map.empty) (NormDim u)
        s2 <- mguDim (PowDim Map.empty) (apply s1 (PowDim t))
        return $ s2 `composeSubst` s1
    )
      `catchError` (\_ -> throwError [(BaseDim (NormDim u), BaseDim (PowDim t))])
mguDim (PowDim u) (NormDim t) =
  do
    -- I can only unify BaseDims and PowDims if they both unify to dimensionless
    s1 <- mguDim (PowDim Map.empty) (PowDim u)
    s2 <- mguDim (NormDim Map.empty) (apply s1 (NormDim t))
    return $ s2 `composeSubst` s1
    `catchError` (\_ -> throwError [(BaseDim (NormDim u), BaseDim (PowDim t))])

mgu :: Type -> Type -> UM Substitution
mgu a b = wrapError a b (mgu' a b)

mgu' :: Type -> Type -> UM Substitution
mgu' (FuncType l r) (FuncType l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s1 `composeSubst` s2)
mgu' (PolyType u) t = varBind u t
mgu' t (PolyType u) = varBind u t
mgu' (BaseDim t) (BaseDim u) = mguDim t u
mgu' (PolyDictType t) (DictType u) = do
  foldM go nullSubst (Map.toList t)
  where
    go :: Substitution -> (T.Text, Type) -> UM Substitution
    go sub (key, type_) = do
      case Map.lookup key u of
        Just currType -> do
          s1 <- mgu (apply sub type_) (apply sub currType)
          return (sub `composeSubst` s1)
        Nothing -> throwError [(PolyDictType t, DictType u)]
mgu' (DictType t) (PolyDictType u) = mgu' (PolyDictType u) (DictType t)
mgu' (ListType t) (ListType u) = mgu t u
mgu' t1 t2 = throwError [(t1, t2)]

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

typeCheck :: TypeCheckState -> [Statement] -> (Maybe (Positioned TypeError), TypeCheckState)
typeCheck tcState statements =
  let (result, _) = runTI (inferLoop tcState statements)
   in case result of
        Right x -> x
        Left err -> (Just err, tcState)
  where
    inferLoop :: TypeCheckState -> [Statement] -> TI (Maybe (Positioned TypeError), TypeCheckState)
    inferLoop state [] = return (Nothing, state)
    inferLoop state (statement : rest) =
      case statement of
        UnitStatement units ->
          let newUnits = tcsUnits state `Set.union` Set.fromList units
           in inferLoop (state {tcsUnits = newUnits}) rest
        AssignmentStatement assignment -> do
          mapPairs <-
            mapM
              ( \a -> do
                  tv <- newTyVar a
                  return (a, Scheme [] tv)
              )
              (assignmentArguments assignment)
          let env = tcsEnv state
              arguments = assignmentArguments assignment
              TypeEnv env' = foldr remove env arguments
              env'' = TypeEnv (env' `Map.union` Map.fromList mapPairs)
          TypeCheckResult s1 t1 ex <- ti (state {tcsEnv = env''}) (assignmentExpression assignment)

          let varType = foldr (\(_, Scheme _ tv) acc -> apply s1 tv `FuncType` acc) (apply s1 t1) mapPairs
              name = assignmentName assignment
              TypeEnv envWithoutName = remove name env
              t' = generalize (apply s1 env) varType
              envWithVar = TypeEnv (Map.insert name t' envWithoutName)
          let newTcState =
                state
                  { tcsEnv = envWithVar,
                    tcsSubs = s1 `composeSubst` tcsSubs state,
                    tcsExecutionExpressions = (name, wrapFunctionArgs arguments ex) OMap.|< tcsExecutionExpressions state
                  }
          inferLoop newTcState rest
        `catchError` (\err -> return (Just err, state))

wrapFunctionArgs :: [T.Text] -> ExecutionExpression -> ExecutionExpression
wrapFunctionArgs (arg : rest) expr = EConstant (ExecutionValueFunc arg (wrapFunctionArgs rest expr))
wrapFunctionArgs [] expr = expr

data TypeCheckResult = TypeCheckResult Substitution Type ExecutionExpression

foldSubst :: Traversable t => t Substitution -> Substitution
foldSubst = foldr composeSubst nullSubst

ti :: TypeCheckState -> Positioned ParseExpression -> TI TypeCheckResult
ti state (Positioned pos expression) =
  let (TypeEnv env) = tcsEnv state
      allowedUnits = tcsUnits state
   in case expression of
        PVariable n ->
          case Map.lookup n env of
            Nothing ->
              case filter ((== n) . InBuilt.funcName) InBuilt.inBuiltFunctions of
                func : _ -> do
                  t <- instantiate (InBuilt.funcType func)
                  return $ TypeCheckResult nullSubst t (EVariable n)
                [] ->
                  throwError $ Positioned pos $ MissingVariableError n
            Just sigma -> do
              t <- instantiate sigma
              return $ TypeCheckResult nullSubst t (EVariable n)
        PConstant (ParseNumber value pdim) -> do
          dimension <- evaluateDimension allowedUnits pdim
          return $ TypeCheckResult nullSubst (BaseDim dimension) (EConstant $ ExecutionValueNumber value)
        PConstant ParseEmptyList -> do
          let emptyListType = Scheme ["a", "t"] $ ListType (BaseDim (NormDim (Map.singleton (PolyDim "a") 1)))
          dim <- instantiate emptyListType
          return $ TypeCheckResult nullSubst dim (EConstant ExecutionValueEmptyList)
        PConstant (ParseRecord record) -> do
          recordEntries <- forM (Map.toList record) $ \(key, el) -> do
            TypeCheckResult sub _type ex <- ti state el
            return (key, (sub, _type, ex))

          let dimension = map (\(key, (_, d, _)) -> (key, d)) recordEntries
              elems = map (\(key, (_, _, value)) -> (key, value)) recordEntries
              substitutions = map (\(_, (sub, _, _)) -> sub) recordEntries
          return $ TypeCheckResult (foldSubst substitutions) (DictType (Map.fromList dimension)) (EConstant $ ExecutionValueDict (Map.fromList elems))
        PBinOp "" e1 e2 ->
          do
            tv <- newTyVar "a"
            TypeCheckResult sub1 type1 ex1 <- ti state e1
            TypeCheckResult sub2 type2 ex2 <- ti (apply sub1 state) e2
            let reason = BinaryOpUnificationReason "" (e1, type1) (e2, type2)
            sub3 <- liftUMtoTI pos reason $ mgu (apply sub2 type1) (FuncType type2 tv)
            return $ TypeCheckResult (sub3 `composeSubst` sub2 `composeSubst` sub1) (apply sub3 tv) (EBinOp "" ex1 ex2)
        PBinOp opName e1 e2 ->
          do
            case filter ((== opName) . InBuilt.opName) InBuilt.inBuiltBinaryOperations of
              [] -> throwError $ Positioned pos $ InternalError $ "ERROR, COULD NOT FIND OPERATION " <> opName
              op : _ -> do
                tv <- newTyVar "a"
                opType <- instantiate (InBuilt.opType op)
                TypeCheckResult s1 t1 ex1 <- ti state e1
                TypeCheckResult s2 t2 ex2 <- ti (apply s1 state) e2
                let reason = BinaryOpUnificationReason opName (e1, t1) (e2, t2)
                s3 <- liftUMtoTI pos reason $ mgu opType (t1 `FuncType` (t2 `FuncType` tv))
                return $ TypeCheckResult (s3 `composeSubst` s2 `composeSubst` s1) (apply s3 tv) (EBinOp opName ex1 ex2)
        PAccess e1 x ->
          do
            tv <- newTyVar "a"
            TypeCheckResult s1 t1 ex1 <- ti state e1
            let reason = AccessUnificationReason (e1, t1) x
            s2 <- liftUMtoTI pos reason $ mgu t1 (PolyDictType (Map.singleton x tv))
            return $ TypeCheckResult (s2 `composeSubst` s1) (apply s2 tv) (EAccess ex1 x)
        PPrefix preOp e1 ->
          case filter ((== preOp) . InBuilt.opName) InBuilt.inBuiltPrefixOperations of
            [] -> throwError $ Positioned pos (MissingVariableError preOp)
            op : _ -> do
              let prefixScheme = InBuilt.opType op
              prefixType <- instantiate prefixScheme
              tv <- newTyVar "a"
              TypeCheckResult s1 t1 ex1 <- ti state e1
              let reason = PrefixOpUnificationReason preOp (e1, t1)
              s2 <- liftUMtoTI pos reason $ mgu prefixType (t1 `FuncType` tv)
              return $ TypeCheckResult (s2 `composeSubst` s1) (apply s2 tv) (ENegate ex1)

evaluateDimension :: Set.Set T.Text -> ParseDimension -> TI Dimension
evaluateDimension allowedUnits dim =
  case dim of
    PowParseDim components ->
      PowDim <$> foldM addToDimensionMap Map.empty components
    NormalParseDim components ->
      NormDim <$> foldM addToDimensionMap Map.empty components
  where
    addToDimensionMap :: Map.Map PrimitiveDim Int -> Positioned ParseDimensionPart -> TI (Map.Map PrimitiveDim Int)
    addToDimensionMap dimMap (Positioned p (ParseDimensionPart name power)) =
      if Set.member name allowedUnits
        then return $ Map.insert (LitDim name) power dimMap
        else throwError $ Positioned p $ MissingUnitError name
