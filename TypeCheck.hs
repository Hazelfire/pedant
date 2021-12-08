-- | TypeChecker for Pedant.
module TypeCheck where

import Control.Monad (forM, liftM2)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Bifunctor as Bifunctor
import Data.Char
import qualified Data.Either as Either
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import qualified Data.Map.Ordered as OMap
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import Debug.Trace
import Parser
import Types

data TypeError = TypeError PositionData String
  deriving (Ord, Eq)

data TypeCheckState = TypeCheckState
  { tcsEnv :: TypeEnv,
    tcsSubs :: Substitution,
    tcsUnits :: Set.Set String,
    tcsExecutionExpressions :: OMap.OMap String ExecutionExpression
  }

imap :: (Int -> a -> b) -> [a] -> [b]
imap f list = _imap f list 0
  where
    _imap f (x : rest) idx =
      f idx x : _imap f rest (idx + 1)
    _imap f [] idx = []

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState = TypeCheckState (TypeEnv Map.empty) nullSubst Set.empty OMap.empty

-- | There are two different types of substitutions. Dimensional and type substitutions.
data Substitution = Substitution
  { subTypes :: Map.Map String Type,
    subDimensions :: Map.Map String Dimension
  }
  deriving (Show)

class Types a where
  ftv :: a -> Set.Set String
  apply :: Substitution -> a -> a

instance Types Dimension where
  ftv (NormDim n) =
    let keys = Map.keys n
     in Set.fromList $ Maybe.mapMaybe polymorphicVar keys
    where
      polymorphicVar :: PrimitiveDim -> Maybe String
      polymorphicVar (PolyPrimDim a) = Just a
      polymorphicVar _ = Nothing
  ftv (PowDim n) = ftv (NormDim n)
  ftv (PolyDim n) = Set.singleton n

  apply s dim =
    case dim of
      NormDim n -> NormDim $ Map.foldlWithKey applyOne Map.empty n
      PowDim n -> PowDim $ Map.foldlWithKey applyOne Map.empty n
      PolyDim key -> case Map.lookup key (subDimensions s) of
        Just substitution -> substitution
        Nothing -> PolyDim key
    where
      applyOne :: Map.Map PrimitiveDim Int -> PrimitiveDim -> Int -> Map.Map PrimitiveDim Int
      applyOne map (LitDim x) power = Map.filter (/= 0) $ Map.unionWith (+) map (Map.singleton (LitDim x) power)
      applyOne map (PolyPrimDim x) power =
        case Map.lookup x (subDimensions s) of
          Just (NormDim substitution) -> combine map (Map.map (* power) substitution)
          Just (PowDim substitution) -> combine map (Map.map (* power) substitution)
          Just (PolyDim newName) -> combine map (Map.singleton (PolyPrimDim newName) power)
          _ -> combine map (Map.singleton (PolyPrimDim x) power)

      combine :: Map.Map PrimitiveDim Int -> Map.Map PrimitiveDim Int -> Map.Map PrimitiveDim Int
      combine a b = Map.filter (/= 0) $ Map.unionWith (+) a b

instance Types Type where
  ftv (PolyType n) = Set.singleton n
  ftv (FuncType x y) = ftv x `Set.union` ftv y
  ftv (DictType x) = Set.unions . map ftv $ Map.elems x
  ftv (ListType x) = ftv x
  ftv (BaseDim x) = ftv x
  ftv (PolyNumericType n x) = Set.insert n (ftv x)
  ftv (PolyDictType x) = Set.unions . map ftv $ Map.elems x

  apply s (PolyType n) =
    case Map.lookup n (subTypes s) of
      Nothing -> PolyType n
      Just x -> apply s x
  apply s (FuncType x y) = FuncType (apply s x) (apply s y)
  apply s (PolyNumericType n d) =
    case Map.lookup n (subTypes s) of
      Just (PolyType n2) ->
        -- You cannot substitute a PolyNumeric for a PolyType. It's too general. Stick to PolyType
        PolyNumericType n2 (apply s d)
      Just (ListType x) ->
        apply s (ListType (BaseDim d))
      Just (BaseDim x) ->
        apply s (BaseDim d)
      _ ->
        PolyNumericType n (apply s d)
  apply s (BaseDim n) =
    BaseDim $ apply s n
  apply s (ListType n) =
    ListType $ apply s n
  apply s (PolyDictType n) =
    PolyDictType (Map.map (apply s) n)
  apply s (DictType n) =
    DictType (Map.map (apply s) n)

data Scheme = Scheme [String] Type

prettyPrintPrimitive :: PrimitiveDim -> String
prettyPrintPrimitive (LitDim s) = s
prettyPrintPrimitive (PolyPrimDim s) = "'" ++ s

prettyPrintDimension :: Dimension -> String
prettyPrintDimension (NormDim dim) =
  if Map.empty == dim
    then "1"
    else unwords $ map (\(name, amount) -> if amount == 1 then prettyPrintPrimitive name else prettyPrintPrimitive name ++ show amount) (List.sortOn (negate . snd) (Map.toList dim))
prettyPrintDimension (PowDim dim) =
  if Map.empty == dim
    then "1"
    else ("^" ++) $ unwords $ map (\(name, amount) -> if amount == 1 then prettyPrintPrimitive name else prettyPrintPrimitive name ++ show amount) (List.sortOn (negate . snd) (Map.toList dim))
prettyPrintDimension (PolyDim dim) = "'" ++ dim

prettyPrintType :: Type -> String
prettyPrintType (BaseDim s) = prettyPrintDimension s
prettyPrintType (PolyNumericType n s) = prettyPrintDimension s
prettyPrintType (DictType d) =
  "{" ++ List.intercalate "," (map (\(key, value) -> key ++ ":" ++ prettyPrintType value) (Map.toAscList d)) ++ "}"
prettyPrintType (ListType s) = "[" ++ prettyPrintType s ++ "]"
prettyPrintType (PolyDictType d) =
  "{|" ++ List.intercalate "," (map (\(key, value) -> key ++ ":" ++ prettyPrintType value) (Map.toAscList d)) ++ "}"
prettyPrintType (PolyType s) = "'" ++ s
prettyPrintType (FuncType x y) =
  prettyPrintType x ++ " -> " ++ prettyPrintType y

prettyPrintScheme :: Scheme -> String
prettyPrintScheme s@(Scheme vars t) =
  let typeNames = imap (\i v -> (v, PolyType [chr (ord 'a' + i)])) vars
      dimNames = imap (\i v -> (v, PolyDim [chr (ord 'a' + i)])) vars
      sub = Substitution {subTypes = Map.fromList typeNames, subDimensions = Map.fromList dimNames}
   in prettyPrintType $ apply sub t

instance Show Scheme where
  show (Scheme [] t) = show t
  show (Scheme args t) = "∀" ++ unwords args ++ ". " ++ show t

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr deleteFromSub s vars) t)
    where
      deleteFromSub :: String -> Substitution -> Substitution
      deleteFromSub key sub =
        Substitution
          { subDimensions = Map.delete key (subDimensions sub),
            subTypes = Map.delete key (subTypes sub)
          }

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv l = Set.unions $ map ftv l

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

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
  deriving (Show)

remove :: String -> TypeEnv -> TypeEnv
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

type TI a = ExceptT TypeError (State TIState) a

runTI :: TI a -> (Either TypeError a, TIState)
runTI t =
  runState (runExceptT t) initTIState
  where
    initTIState = TIState {tiSupply = 0}

newTyVar :: String -> TI Type
newTyVar prefix =
  do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (PolyType (prefix ++ show (tiSupply s)))

newTyDimension :: String -> TI Dimension
newTyDimension prefix =
  do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (PolyDim $ prefix ++ show (tiSupply s))

newTyPrimDim :: String -> TI PrimitiveDim
newTyPrimDim prefix =
  do
    s <- get
    put s {tiSupply = tiSupply s + 1}
    return (PolyPrimDim $ prefix ++ show (tiSupply s))

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

-- | Attempts to find a unification between dimensions
mguDim :: PositionData -> Dimension -> Dimension -> TI Substitution
mguDim pos (PolyDim u) t = return $ nullSubst {subDimensions = Map.singleton u t}
mguDim pos t (PolyDim u) = return $ nullSubst {subDimensions = Map.singleton u t}
mguDim pos (NormDim t) (NormDim u) =
  if u == t
    then return nullSubst
    else -- unifying dimensions is a bit tricky, and this method is not perfect and leaves out some possible (but rare) unifications

      let dividedOut = Map.filter (/= 0) $ Map.unionWith (-) t u
          polyDim =
            Maybe.mapMaybe
              ( \(k, v) ->
                  case (k, v) of
                    (PolyPrimDim d, 1) -> Just (d, 1)
                    (PolyPrimDim d, -1) -> Just (d, -1)
                    _ -> Nothing
              )
              (Map.toList dividedOut)
       in case polyDim of
            (firstDim, power) : _ ->
              let withoutPolyVar = Map.delete (PolyPrimDim firstDim) dividedOut
                  dividedByPower = Map.map (`quot` power) withoutPolyVar
               in return $ nullSubst {subDimensions = Map.singleton firstDim (NormDim dividedByPower)}
            [] ->
              throwError $ TypeError pos $ "Cannot unify " ++ show (NormDim t) ++ " and " ++ show (NormDim u)
mguDim pos (PowDim t) (PowDim u) =
  if u == t
    then return nullSubst
    else
      let dividedOut = Map.filter (/= 0) $ Map.unionWith (-) t u
          polyDim =
            Maybe.mapMaybe
              ( \(k, v) ->
                  case (k, v) of
                    (PolyPrimDim d, 1) -> Just d
                    _ -> Nothing
              )
              (Map.toList dividedOut)
       in case polyDim of
            firstDim : _ ->
              let withoutPolyVar = Map.delete (PolyPrimDim firstDim) dividedOut
               in return $ nullSubst {subDimensions = Map.singleton firstDim (PowDim withoutPolyVar)}
            [] ->
              throwError $ TypeError pos $ "Cannot unify " ++ show (PowDim t) ++ " and " ++ show (PowDim u)
mguDim pos (NormDim u) (PowDim t) = do
  -- I can only unify BaseDims and PowDims if they both unify to dimensionless
  ( do
      s1 <- mguDim pos (NormDim Map.empty) (NormDim u)
      s2 <- mguDim pos (PowDim Map.empty) (apply s1 (PowDim t))
      return $ s2 `composeSubst` s1
    )
    `catchError` (\_ -> throwError $ TypeError pos $ "Cannot unify " ++ show (NormDim u) ++ " and " ++ show (PowDim t))
mguDim pos (PowDim u) (NormDim t) =
  do
    -- I can only unify BaseDims and PowDims if they both unify to dimensionless
    s1 <- mguDim pos (PowDim Map.empty) (PowDim u)
    s2 <- mguDim pos (NormDim Map.empty) (apply s1 (NormDim t))
    return $ s2 `composeSubst` s1
    `catchError` (\_ -> throwError $ TypeError pos $ "Cannot unify " ++ show (PowDim u) ++ " and " ++ show (NormDim t))

mgu :: PositionData -> Type -> Type -> TI Substitution
mgu pos a b = wrapError a b (mgu' pos a b)

mgu' :: PositionData -> Type -> Type -> TI Substitution
mgu' p (FuncType l r) (FuncType l' r') = do
  s1 <- mgu p l l'
  s2 <- mgu p (apply s1 r) (apply s1 r')
  return (s1 `composeSubst` s2)
mgu' p (PolyType u) t = varBind p u t
mgu' p t (PolyType u) = varBind p u t
mgu' p (BaseDim t) (BaseDim u) = mguDim p t u
mgu' p (PolyNumericType n1 t1) (PolyNumericType n2 t2) = do
  sub <- mguDim p t1 t2
  if n1 == n2
    then return sub
    else return $ sub {subTypes = Map.singleton n1 (PolyNumericType n2 t2)}
mgu' p (PolyNumericType n t) (BaseDim u) = do
  sub <- mguDim p t u
  return $ sub {subTypes = Map.singleton n (BaseDim u)}
mgu' p (BaseDim t) (PolyNumericType n u) = do
  sub <- mguDim p t u
  return $ sub {subTypes = Map.singleton n (BaseDim u)}
mgu' p (PolyDictType t) (DictType u) = do
  foldM go nullSubst (Map.toList t)
  where
    go :: Substitution -> (String, Type) -> TI Substitution
    go sub (key, type_) = do
      case Map.lookup key u of
        Just currType -> do
          s1 <- mgu p (apply sub type_) (apply sub currType)
          return (sub `composeSubst` s1)
        Nothing -> throwError $ TypeError p $ show (DictType u) ++ " is missing key " ++ key
mgu' p (DictType t) (PolyDictType u) = mgu' p (PolyDictType u) (DictType t)
mgu' pos t1 t2 = throwError $ TypeError pos $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

wrapError :: Type -> Type -> TI Substitution -> TI Substitution
wrapError t1 t2 child = child `catchError` (\(TypeError pos err) -> throwError $ TypeError pos $ err ++ "\n while trying to unify " ++ show t1 ++ " and " ++ show t2)

varBind :: PositionData -> String -> Type -> TI Substitution
varBind p u t
  | t == PolyType u = return nullSubst
  | u `Set.member` ftv t =
    throwError $
      TypeError p $
        "occurs check fails: " ++ u
          ++ " vs. "
          ++ show t
  | otherwise = return (nullSubst {subTypes = Map.singleton u t})

typeCheck :: TypeCheckState -> [Statement] -> (Maybe TypeError, TypeCheckState)
typeCheck tcState statements =
  let (result, _) = runTI (inferLoop tcState statements)
   in case result of
        Right result -> result
        Left err -> (Just err, tcState)
  where
    inferLoop :: TypeCheckState -> [Statement] -> TI (Maybe TypeError, TypeCheckState)
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
              TypeEnv env' = remove name env
              t' = generalize (apply s1 env) varType
              envWithVar = TypeEnv (Map.insert name t' env')
          let tcState =
                state
                  { tcsEnv = envWithVar,
                    tcsSubs = s1 `composeSubst` tcsSubs state,
                    tcsExecutionExpressions = (name, wrapFunctionArgs arguments ex) OMap.|< tcsExecutionExpressions state
                  }
          inferLoop tcState rest
        `catchError` (\err -> return (Just err, state))

wrapFunctionArgs :: [String] -> ExecutionExpression -> ExecutionExpression
wrapFunctionArgs (arg : rest) expr = EConstant (ExecutionValueFunc arg (wrapFunctionArgs rest expr))
wrapFunctionArgs [] expr = expr

data TypeCheckResult = TypeCheckResult
  { tiSub :: Substitution,
    tiType :: Type,
    tiExecExpression :: ExecutionExpression
  }

foldSubst :: Traversable t => t Substitution -> Substitution
foldSubst = foldr composeSubst nullSubst

ti :: TypeCheckState -> PositionedExpression -> TI TypeCheckResult
ti state (Positioned pos expression) =
  let (TypeEnv env) = tcsEnv state
      allowedUnits = tcsUnits state
   in case expression of
        PVariable n ->
          case Map.lookup n env of
            Nothing ->
              case inBuiltFunctions n of
                Just scheme -> do
                  t <- instantiate scheme
                  return $ TypeCheckResult nullSubst t (EVariable n)
                Nothing ->
                  throwError $ TypeError pos $ "unbound variable: " ++ n
            Just sigma -> do
              t <- instantiate sigma
              return $ TypeCheckResult nullSubst t (EVariable n)
        PConstant (ParseNumber value pdim) -> do
          dimension <- evaluateDimension allowedUnits pdim
          return $ TypeCheckResult nullSubst (BaseDim dimension) (EConstant $ ExecutionValueNumber value)
        PConstant (ParseList list) -> do
          evaluations <- mapM (ti state) list
          case evaluations of
            [] -> throwError $ TypeError pos "Cannot have empty list"
            TypeCheckResult substitutions _type value : rest ->
              if all ((== _type) . tiType) rest
                then
                  let substitutions = foldSubst (map tiSub evaluations)
                   in return $ TypeCheckResult substitutions (ListType _type) (EConstant $ ExecutionValueList (value : map tiExecExpression rest))
                else throwError $ TypeError pos "All items in a list must have the same units"
        PConstant (ParseRecord record) -> do
          recordEntries <- forM (Map.toList record) $ \(key, elem) -> do
            TypeCheckResult sub _type ex <- ti state elem
            return (key, (sub, _type, ex))

          let dimension = map (\(key, (_, dimension, _)) -> (key, dimension)) recordEntries
              elems = map (\(key, (_, _, value)) -> (key, value)) recordEntries
              substitutions = map (\(key, (sub, _, _)) -> sub) recordEntries
          return $ TypeCheckResult (foldSubst substitutions) (DictType (Map.fromList dimension)) (EConstant $ ExecutionValueDict (Map.fromList elems))
        PBinOp App e1 e2 ->
          do
            tv <- newTyVar "a"
            TypeCheckResult sub1 type1 ex1 <- ti state e1
            TypeCheckResult sub2 type2 ex2 <- ti (apply sub1 state) e2
            sub3 <-
              mgu pos (apply sub2 type1) (FuncType type2 tv)
                `catchError` (\(TypeError pos err) -> throwError $ TypeError pos $ err ++ "\n while trying to apply " ++ show type1 ++ " to " ++ show type2 ++ " in " ++ show (PBinOp App e1 e2) ++ " original state " ++ show (tcsSubs state) ++ " sub1 " ++ show sub1 ++ " sub 2 " ++ show sub2 ++ " and when applied " ++ show (apply sub2 type1))
            return $ TypeCheckResult (sub3 `composeSubst` sub2 `composeSubst` sub1) (apply sub3 tv) (EBinOp App ex1 ex2)
        PBinOp op e1 e2 ->
          do
            tv <- newTyVar "a"
            opType <- instantiate (opDimension op)
            TypeCheckResult s1 t1 ex1 <- ti state e1
            TypeCheckResult s2 t2 ex2 <- ti (apply s1 state) e2
            s3 <- mgu pos opType (t1 `FuncType` (t2 `FuncType` tv))
            return $ TypeCheckResult (s3 `composeSubst` s2 `composeSubst` s1) (apply s3 tv) (EBinOp op ex1 ex2)
        PAccess e1 x ->
          do
            tv <- newTyVar "a"
            TypeCheckResult s1 t1 ex1 <- ti state e1
            s2 <- mgu pos t1 (PolyDictType (Map.singleton x tv))
            return $ TypeCheckResult (s2 `composeSubst` s1) (apply s2 tv) (EAccess ex1 x)
        PNegate e1 ->
          do
            let negateScheme = Scheme ["a"] $ normalDimPoly "a" `FuncType` normalDimPoly "a"
            negateType <- instantiate negateScheme
            tv <- newTyVar "a"
            TypeCheckResult s1 t1 ex1 <- ti state e1
            s2 <- mgu pos negateType (t1 `FuncType` tv)
            return $ TypeCheckResult (s2 `composeSubst` s1) (apply s2 tv) (ENegate ex1)

evaluateDimension :: Set.Set String -> ParseDimension -> TI Dimension
evaluateDimension allowedUnits dim =
  case dim of
    PowParseDim components ->
      PowDim <$> foldM addToDimensionMap Map.empty components
    NormalParseDim components ->
      NormDim <$> foldM addToDimensionMap Map.empty components
  where
    addToDimensionMap :: Map.Map PrimitiveDim Int -> Positioned ParseDimensionPart -> TI (Map.Map PrimitiveDim Int)
    addToDimensionMap map (Positioned p (ParseDimensionPart name power)) =
      if Set.member name allowedUnits
        then return $ Map.insert (LitDim name) power map
        else throwError $ TypeError p (concat ["unit ", name, " not declared. Try adding a \"unit ", name, "\" statement before this line"])

normalDimPoly :: String -> Type
normalDimPoly name = PolyNumericType "t" $ NormDim $ Map.singleton (PolyPrimDim name) 1

powerDimPoly :: String -> Type
powerDimPoly name = PolyNumericType "t" $ PowDim $ Map.singleton (PolyPrimDim name) 1

normalDim :: [(PrimitiveDim, Int)] -> Type
normalDim powers = PolyNumericType "t" $ NormDim $ Map.fromList powers

powerDim :: [(PrimitiveDim, Int)] -> Type
powerDim powers = PolyNumericType "t" $ PowDim $ Map.fromList powers

inBuiltFunctions :: String -> Maybe Scheme
inBuiltFunctions "ln" = Just $ Scheme ["t", "a"] $ powerDimPoly "a" `FuncType` normalDimPoly "a"
inBuiltFunctions _ = Nothing

opDimension :: Operation -> Scheme
opDimension Add = Scheme ["a", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "a" `FuncType` normalDimPoly "a")
opDimension Sub = Scheme ["a", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "a" `FuncType` normalDimPoly "a")
opDimension Mult = Scheme ["a", "b", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` normalDim [(PolyPrimDim "a", 1), (PolyPrimDim "b", 1)])
opDimension Div = Scheme ["a", "b", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` normalDim [(PolyPrimDim "a", 1), (PolyPrimDim "b", -1)])
opDimension Power = Scheme ["a", "b", "t"] $ powerDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` powerDim [(PolyPrimDim "a", 1), (PolyPrimDim "b", 1)])
opDimension App = Scheme ["a", "b", "t"] $ ((PolyType "a" `FuncType` PolyType "b") `FuncType` PolyType "a") `FuncType` PolyType "b"
