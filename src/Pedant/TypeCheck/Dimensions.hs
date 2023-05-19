module TypeCheck.Dimensions where

import qualified Pedant.Types as Types

-- | Attempts to find a unification between dimensions
mguDim :: Types.Dimension -> Types.Dimension -> UM Types.Substitution
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