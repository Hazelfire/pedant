module Pedant.TypeCheck.Expressions (inferType) where

import qualified Pedant.TypeCheck.Types as TC

inferType :: Parser.Positioned Parser.Expression -> T TypeCheckResult
inferType state (Parser.Positioned pos expression) =
  let (TypeEnv env) = tcsEnv state
      allowedUnits = Set.filter (\(VariableName moduleName _) -> moduleName == tcsCurrentModule state) $ tcsUnits state
   in case expression of
        -- We got a variable
        Parser.Variable n ->
          -- Lookup variable in type environment
          case OMap.lookup (VariableName (tcsCurrentModule state) n) env of
            Nothing ->
              case filter ((== n) . InBuilt.funcName) InBuilt.inBuiltFunctions of
                func : _ -> do
                  let t = InBuilt.funcType func
                  return $ TypeCheckResult nullSubst t (EInternalFunc $ InBuilt.funcDef func)
                [] ->
                  throwError $ Parser.Positioned pos $ MissingVariableError n
            Just vi -> do
              t <- variableInfoScheme vi
              return $ TypeCheckResult nullSubst t (EVariable (VariableName (tcsCurrentModule state) n))
        Parser.Number value pdim -> do
          dimension <- evaluateDimension (Set.map (\(VariableName _ name) -> name) allowedUnits) pdim
          return $ TypeCheckResult nullSubst (BaseDim dimension) (EConstant $ ExecutionValueNumber value)
        Parser.List list -> do
          let emptyListType = Scheme ["a", "t"] $ ListType (BaseDim (NormDim (Map.singleton (PolyDim "a") 1)))
          dim <- instantiate emptyListType
          return $ TypeCheckResult nullSubst dim (EConstant (ExecutionValueList ))
        Parser.Record record -> do
          recordEntries <- forM (Map.toList record) $ \(key, el) -> do
            TypeCheckResult sub _type ex <- ti state el
            return (key, (sub, _type, ex))

          let dimension = map (\(key, (_, d, _)) -> (key, d)) recordEntries
              elems = map (\(key, (_, _, value)) -> (key, value)) recordEntries
              substitutions = map (\(_, (sub, _, _)) -> sub) recordEntries
          return $ TypeCheckResult (foldSubst substitutions) (DictType (Map.fromList dimension)) (EConstant $ ExecutionValueDict (Map.fromList elems))
        Parser.BinOp "" e1 e2 ->
          do
            tv <- newTyVar "a"
            TypeCheckResult sub1 type1 ex1 <- ti state e1
            TypeCheckResult sub2 type2 ex2 <- ti (apply sub1 state) e2
            let reason = BinaryOpUnificationReason "" (e1, type1) (e2, type2)
            sub3 <- liftUMtoTI pos reason $ mgu (apply sub2 type1) (FuncType type2 tv)
            return $ TypeCheckResult (sub3 `composeSubst` sub2 `composeSubst` sub1) (apply sub3 tv) (EBinOp "" ex1 ex2)
        Parser.BinOp opName e1 e2 ->
          do
            case filter ((== opName) . InBuilt.opName) InBuilt.inBuiltBinaryOperations of
              [] -> throwError $ Parser.Positioned pos $ InternalError $ "ERROR, COULD NOT FIND OPERATION " <> opName
              op : _ -> do
                tv <- newTyVar "a"
                opType <- instantiate (InBuilt.opType op)
                TypeCheckResult s1 t1 ex1 <- ti state e1
                TypeCheckResult s2 t2 ex2 <- ti (apply s1 state) e2
                let reason = BinaryOpUnificationReason opName (e1, t1) (e2, t2)
                s3 <- liftUMtoTI pos reason $ mgu opType (t1 `FuncType` (t2 `FuncType` tv))
                return $ TypeCheckResult (s3 `composeSubst` s2 `composeSubst` s1) (apply s3 tv) (EBinOp opName ex1 ex2)
        Parser.Access e1 x ->
          do
            tv <- newTyVar "a"
            TypeCheckResult s1 t1 ex1 <- ti state e1
            let reason = AccessUnificationReason (e1, t1) x
            s2 <- liftUMtoTI pos reason $ mgu t1 (PolyDictType (Map.singleton x tv))
            return $ TypeCheckResult (s2 `composeSubst` s1) (apply s2 tv) (EAccess ex1 x)
        Parser.Prefix preOp e1 ->
          case filter ((== preOp) . InBuilt.opName) InBuilt.inBuiltPrefixOperations of
            [] -> throwError $ Parser.Positioned pos (MissingVariableError preOp)
            op : _ -> do
              let prefixScheme = InBuilt.opType op
              prefixType <- instantiate prefixScheme
              tv <- newTyVar "a"
              TypeCheckResult s1 t1 ex1 <- ti state e1
              let reason = PrefixOpUnificationReason preOp (e1, t1)
              s2 <- liftUMtoTI pos reason $ mgu prefixType (t1 `FuncType` tv)
              return $ TypeCheckResult (s2 `composeSubst` s1) (apply s2 tv) (ENegate ex1)

evaluateDimension :: Set.Set T.Text -> Parser.Dimension -> TI Dimension
evaluateDimension allowedUnits dim =
  case dim of
    Parser.PowParseDim components ->
      PowDim <$> foldM addToDimensionMap Map.empty components
    Parser.NormalParseDim components ->
      NormDim <$> foldM addToDimensionMap Map.empty components
  where
    addToDimensionMap :: Map.Map PrimitiveDim Int -> Parser.Positioned Parser.DimensionPart -> TI (Map.Map PrimitiveDim Int)
    addToDimensionMap dimMap (Parser.Positioned p (Parser.DimensionPart name power)) =
      if Set.member name allowedUnits
        then return $ Map.insert (LitDim name) power dimMap
        else throwError $ Parser.Positioned p $ MissingUnitError name

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