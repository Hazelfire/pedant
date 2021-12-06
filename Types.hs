module Types
  ( Dimension (..),
    PrimitiveDim (..),
    Type (..),
    baseUnitsDim,
    Operation (..),
    baseDimension,
    baseUnits,
    ExecutionExpression (..),
    PedantParseError (..),
    ExecutionValue (..),
    NumericValue (..),
    dimRecip,
    typeMult,
    dimensionless,
  )
where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

-- | Defining a shallow embedding for a typed number.
--   A typed number is a number with units. It must follow
--   the rules of dimensional analysis
data NumericValue
  = NumberValue Double
  | -- | A number and the dimension of the number
    ListValue [NumericValue]
  | DictValue (Map.Map String NumericValue)
  | FuncValue String ExecutionExpression

lift2Numeric :: (Double -> Double -> Double) -> NumericValue -> NumericValue -> NumericValue
lift2Numeric op a b =
  case (a, b) of
    (NumberValue a, NumberValue b) -> NumberValue (op a b)
    (NumberValue a, ListValue b) -> ListValue (map (lift2Numeric op $ NumberValue a) b)
    (ListValue a, NumberValue b) -> ListValue (map (\x -> lift2Numeric op x (NumberValue b)) a)
    (ListValue a, ListValue b) -> ListValue (zipWith (lift2Numeric op) a b)
    (a, _) -> a

liftNumeric :: (Double -> Double) -> NumericValue -> NumericValue
liftNumeric op a =
  case a of
    NumberValue x -> NumberValue $ op x
    ListValue list -> ListValue (map (liftNumeric op) list)
    DictValue a -> DictValue a
    FuncValue a b -> FuncValue a b

instance Num NumericValue where
  (*) = lift2Numeric (*)

  (+) = lift2Numeric (+)

  (-) = lift2Numeric (-)

  negate = liftNumeric negate

  abs = liftNumeric abs

  signum = liftNumeric signum
  fromInteger = NumberValue . fromInteger

instance Fractional NumericValue where
  fromRational = NumberValue . fromRational
  (/) = lift2Numeric (/)

instance Floating NumericValue where
  pi = NumberValue pi
  exp = liftNumeric exp
  log = liftNumeric log
  sin = liftNumeric sin
  cos = liftNumeric cos
  asin = liftNumeric asin
  acos = liftNumeric acos
  atan = liftNumeric atan
  sinh = liftNumeric sinh
  cosh = liftNumeric cosh
  asinh = liftNumeric asinh
  acosh = liftNumeric acosh
  atanh = liftNumeric atanh

data PrimitiveDim
  = -- | Literal dimension, such as years
    LitDim String
  | -- | Polymorphic dimension, such as <a>
    PolyPrimDim String
  deriving (Eq, Ord)

data Dimension
  = -- | a non-power dimension (such as years)
    NormDim (Map.Map PrimitiveDim Int)
  | -- | A power dimension (such as ^years-1)
    PowDim (Map.Map PrimitiveDim Int)
  | -- | A dimension that we are unsure of
    PolyDim String
  deriving (Eq)

data Type
  = -- | An actual dimension, such as people years-1
    BaseDim Dimension
  | -- | A list of a dimension, such as [years]
    ListType Type
  | -- | Either a list or a base dimension.
    PolyNumericType String Dimension
  | -- | A dictionary of dimensions, such as {x:meters,y:meters}
    DictType (Map.Map String Type)
  | -- | A polymorphic dictionary (a dictionary that contains these keys or more). Such as {|x:meters,y:meters}
    PolyDictType (Map.Map String Type)
  | -- | A Function. Such as years -> meters
    FuncType Type Type
  | -- | A Polymorphic Type. A type that could be anything
    PolyType String
  deriving (Eq)

isDimensionless :: Type -> Bool
isDimensionless (BaseDim (NormDim x)) = Map.empty == x
isDimensionless _ = False

instance Show PrimitiveDim where
  show (LitDim s) = s
  show (PolyPrimDim s) = "prim<" ++ s ++ ">"

instance Show Dimension where
  show (NormDim dim) =
    if Map.size dim == 0
      then "dimensionless"
      else unwords $ map (\(name, amount) -> if amount == 1 then show name else show name ++ show amount) (List.sortOn (negate . snd) (Map.toList dim))
  show (PolyDim dim) =
    "dim<" ++ dim ++ ">"
  show (PowDim dim) =
    if Map.size dim == 1
      then "^" ++ show (NormDim dim)
      else "^(" ++ show (NormDim dim) ++ ")"

instance Show Type where
  show (BaseDim dim) = show dim
  show (ListType dim) =
    "[" ++ show dim ++ "]"
  show (PolyNumericType name dim) = "numeric<" ++ name ++ "," ++ show dim ++ ">"
  show (DictType dim) =
    "{" ++ List.intercalate "," (map (\(key, value) -> key ++ ":" ++ show value) (Map.toAscList dim)) ++ "}"
  show (PolyDictType dim) =
    "{|" ++ List.intercalate "," (map (\(key, value) -> key ++ ":" ++ show value) (Map.toAscList dim)) ++ "}"
  show (FuncType dimArg dimVal) =
    show dimArg ++ "->" ++ show dimVal
  show (PolyType a) =
    "type<" ++ a ++ ">"

-- | Base Dimension returns the underlying normal dimension for lists. This
--   is used to check whether a dimension can be multiplied or added
baseDimension :: Type -> Type
baseDimension (ListType a) = baseDimension a
baseDimension x = x

-- | The reciprocal of a dimension
dimRecip :: Type -> Either String Type
dimRecip (BaseDim (NormDim x)) = Right . BaseDim . NormDim $ Map.map negate x
dimRecip (ListType x) = ListType <$> dimRecip x
dimRecip x = Left $ "Cannot find recip of " ++ show x

-- | Multiplies two dimensions together
typeMult :: Type -> Type -> Either String Type
typeMult (BaseDim (NormDim a)) (BaseDim (NormDim b)) = Right . BaseDim . NormDim $ Map.filter (/= 0) $ Map.unionWith (+) a b
typeMult (ListType a) (ListType b) = ListType <$> typeMult a b
typeMult (ListType a) b = ListType <$> typeMult a b
typeMult a (ListType b) = ListType <$> typeMult a b
typeMult x y = Left $ "Cannot multiply " ++ show x ++ " to " ++ show y

dimensionless :: Type
dimensionless = BaseDim $ NormDim Map.empty

baseUnitPrim :: PrimitiveDim -> Maybe String
baseUnitPrim (LitDim x) = Just x
baseUnitPrim (PolyPrimDim x) = Nothing

baseUnitsDim :: Dimension -> Set.Set String
baseUnitsDim (NormDim a) = Set.fromList . Maybe.mapMaybe baseUnitPrim $ Map.keys a
baseUnitsDim (PowDim a) = Set.fromList . Maybe.mapMaybe baseUnitPrim $ Map.keys a
baseUnitsDim (PolyDim a) = Set.empty

-- | Base Units. Which units make up the type. Used for checking whether
--   units have been declared
baseUnits :: Type -> Set.Set String
baseUnits (BaseDim a) = baseUnitsDim a
baseUnits (ListType a) = baseUnits a
baseUnits (DictType a) = Set.unions (map baseUnits $ Map.elems a)
baseUnits (PolyDictType a) = Set.unions (map baseUnits $ Map.elems a)
baseUnits (PolyNumericType _ a) = baseUnitsDim a
baseUnits (FuncType a b) = baseUnits a `Set.union` baseUnits b
baseUnits (PolyType _) = Set.empty

instance Show NumericValue where
  show (NumberValue val) = show val
  show (ListValue val) = "[" ++ List.intercalate ", " (map show val) ++ "]"
  show (DictValue val) = "{" ++ List.intercalate ", " (map (\(key, value) -> key ++ "=" ++ show value) (Map.toAscList val)) ++ "}"
  show (FuncValue arg exp) = arg ++ " -> " ++ show exp

data ExecutionValue
  = ExecutionValueNumber Double
  | ExecutionValueList [ExecutionExpression]
  | ExecutionValueDict (Map.Map String ExecutionExpression)
  | ExecutionValueFunc String ExecutionExpression
  deriving (Show)

data ExecutionExpression
  = EBinOp Operation ExecutionExpression ExecutionExpression
  | EVariable String
  | EAccess ExecutionExpression String
  | EConstant ExecutionValue
  | ENegate ExecutionExpression
  deriving (Show)

data Operation = Add | Sub | Mult | Div | App | Power
  deriving (Show)

data PedantParseError = PedantParseError
  { ppeErrString :: String,
    ppeColumn :: Int,
    ppeRow :: Int,
    ppePrint :: String
  }
