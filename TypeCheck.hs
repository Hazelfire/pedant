-- | TypeChecker for Pedant.
module TypeCheck where

import qualified Data.Bifunctor as Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import Data.Map.Ordered ((|<))
import qualified Data.Map.Ordered as OMap
import qualified Data.Set as Set
import Data.Text (Text)
import Parser
import Types

data TypeError = TypeError PositionData String

data TypeCheckState = TypeCheckState
  { tcsVariables :: OMap.OMap String (Dimension, ExecutionExpression),
    tcsUnits :: Set.Set String
  }

emptyTypeCheckState :: TypeCheckState
emptyTypeCheckState = TypeCheckState OMap.empty Set.empty

typeCheck :: TypeCheckState -> [Statement] -> Either TypeError TypeCheckState
typeCheck tcState (statement : rest) =
  case statement of
    UnitStatement units -> typeCheck (tcState {tcsUnits = Set.union (tcsUnits tcState) (Set.fromList units)}) rest
    AssignmentStatement (Assignment name expr) ->
      case typeCheckExpression tcState expr of
        Left err -> Left err
        Right value -> typeCheck (tcState {tcsVariables = (name, value) |< tcsVariables tcState}) rest
typeCheck tcState [] = Right tcState

typeCheckExpression :: TypeCheckState -> PositionedExpression -> Either TypeError (Dimension, ExecutionExpression)
typeCheckExpression tcState (PositionedExpression pos expression) =
  let toTypeError = Bifunctor.first (TypeError pos)
   in case expression of
        PBinOp Mult x y -> do
          (xdim, xnum) <- typeCheckExpression tcState x
          (ydim, ynum) <- typeCheckExpression tcState y
          dimension <- toTypeError (dimMult xdim ydim)
          return (dimension, EBinOp Mult xnum ynum)
        PBinOp Div x y -> do
          (xdim, xnum) <- typeCheckExpression tcState x
          (ydim, ynum) <- typeCheckExpression tcState y
          reciprocated <- toTypeError $ dimRecip ydim
          dimension <- toTypeError (dimMult xdim reciprocated)
          return (dimension, EBinOp Div xnum ynum)
        PBinOp Add x y -> do
          (xdim, xnum) <- typeCheckExpression tcState x
          (ydim, ynum) <- typeCheckExpression tcState y
          if baseDimension xdim == baseDimension ydim
            then return (xdim, EBinOp Add xnum ynum)
            else Left . TypeError pos $ "Could not add " ++ show xdim ++ " and " ++ show ydim
        PBinOp Sub x y -> do
          (xdim, xnum) <- typeCheckExpression tcState x
          (ydim, ynum) <- typeCheckExpression tcState y
          if baseDimension xdim == baseDimension ydim
            then return (xdim, EBinOp Sub xnum ynum)
            else Left . TypeError pos $ "Could not subtract " ++ show xdim ++ " and " ++ show ydim
        PBinOp Power x y -> do
          (xdim, xnum) <- typeCheckExpression tcState x
          (ydim, ynum) <- typeCheckExpression tcState y
          case xdim of
            PowDim childDim -> do
              dimension <- toTypeError $ dimMult (NormDim childDim) ydim
              return (dimension, EBinOp Power xnum ynum)
            _ ->
              if dimensionless xdim && dimensionless ydim
                then return (xdim, EBinOp Power xnum ynum)
                else Left $ TypeError pos $ "Could not power " ++ show xdim ++ " to " ++ show ydim ++ " base needs to be power type"
        PBinOp App (PositionedExpression _ (PVariable "ln")) x -> do
          (xdim, xnum) <- typeCheckExpression tcState x
          case xdim of
            PowDim childDim ->
              return (NormDim childDim, EBinOp App (EVariable "ln") xnum)
            NormDim dim ->
              if dim == Map.empty
                then return (NormDim dim, EBinOp App (EVariable "ln") xnum)
                else Left $ TypeError pos $ "Count not take log of " ++ show xdim ++ " must be power type or dimensionless"
            _ ->
              Left $ TypeError pos $ "Count not take log of " ++ show xdim ++ " must be power type or dimensionless"
        PBinOp App (PositionedExpression _ (PVariable name)) x ->
          Left $ TypeError pos "No such function"
        PBinOp App (PositionedExpression _ (PConstant val)) x -> Left . TypeError pos $ "Constants can not be used as functions"
        PBinOp App (PositionedExpression _ (PNegate val)) x -> Left . TypeError pos $ "Cannot apply negation"
        PBinOp App _ x -> Left . TypeError pos $ "Could not evalute function"
        PVariable name ->
          case OMap.lookup name (tcsVariables tcState) of
            Just (dim, _) ->
              return (dim, EVariable name)
            Nothing -> Left . TypeError pos $ "Could not find variable " ++ name
        PConstant (TypedNumber value dim) ->
          let base = baseUnits dim
              missingUnits = Set.difference base (tcsUnits tcState)
           in if Set.size missingUnits == 0
                then return (dim, EConstant $ ExecutionValueNumber value)
                else Left $ TypeError pos (concat ["units ", unwords (Set.toList missingUnits), " not declared. Try adding a \"unit ", unwords (Set.toList missingUnits), "\" statement before this line"])
        PConstant (TypedList list) -> do
          evaluations <- mapM (typeCheckExpression tcState) list
          case evaluations of
            [] -> Left $ TypeError pos "Cannot have empty list"
            (dimension, value) : rest ->
              if all ((== dimension) . fst) rest
                then return (ListDim dimension, EConstant $ ExecutionValueList (value : map snd rest))
                else Left $ TypeError pos "All items in a list must have the same units"
        PNegate expr -> do
          (dim, num) <- typeCheckExpression tcState expr
          return (dim, ENegate num)
