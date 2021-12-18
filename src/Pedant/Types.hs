{-# LANGUAGE OverloadedStrings #-}

module Pedant.Types
  ( Dimension (..),
    PrimitiveDim (..),
    Scheme (..),
    Type (..),
    baseUnitsDim,
    Operation (..),
    baseDimension,
    baseUnits,
    ExecutionExpression (..),
    PedantParseError (..),
    ExecutionValue (..),
    NumericValue (..),
    typeMult,
    dimensionless,
    PrettyPrint (..),
    Substitution (..),
    Types (..),
  )
where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as T

class PrettyPrint a where
  pPrint :: a -> T.Text

-- | Defining a shallow embedding for a typed number.
--   A typed number is a number with units. It must follow
--   the rules of dimensional analysis
data NumericValue
  = NumberValue Double
  | -- | A number and the dimension of the number
    ListValue [NumericValue]
  | DictValue (Map.Map T.Text NumericValue)
  | FuncValue T.Text ExecutionExpression

lift2Numeric :: (Double -> Double -> Double) -> NumericValue -> NumericValue -> NumericValue
lift2Numeric op a b =
  case (a, b) of
    (NumberValue x, NumberValue y) -> NumberValue (op x y)
    (NumberValue x, ListValue y) -> ListValue (map (lift2Numeric op $ NumberValue x) y)
    (ListValue x, NumberValue y) -> ListValue (map (\z -> lift2Numeric op z (NumberValue y)) x)
    (ListValue x, ListValue y) -> ListValue (zipWith (lift2Numeric op) x y)
    (x, _) -> x

liftNumeric :: (Double -> Double) -> NumericValue -> NumericValue
liftNumeric op a =
  case a of
    NumberValue x -> NumberValue $ op x
    ListValue list -> ListValue (map (liftNumeric op) list)
    DictValue x -> DictValue x
    FuncValue x y -> FuncValue x y

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
    LitDim T.Text
  | -- | Polymorphic dimension, such as <a>
    PolyDim T.Text
  deriving (Eq, Ord)

data Dimension
  = -- | a non-power dimension (such as years)
    NormDim (Map.Map PrimitiveDim Int)
  | -- | A power dimension (such as ^years-1)
    PowDim (Map.Map PrimitiveDim Int)
  deriving (Eq)

data Type
  = -- | An actual dimension, such as people years-1
    BaseDim Dimension
  | -- | A list of a dimension, such as [years]
    ListType Type
  | -- | A dictionary of dimensions, such as {x:meters,y:meters}
    DictType (Map.Map T.Text Type)
  | -- | A polymorphic dictionary (a dictionary that contains these keys or more). Such as {|x:meters,y:meters}
    PolyDictType (Map.Map T.Text Type)
  | -- | A Function. Such as years -> meters
    FuncType Type Type
  | -- | A Polymorphic Type. A type that could be anything
    PolyType T.Text
  deriving (Eq)

-- Show instances for dimensions. Shows internal details and may be
-- difficult to read.
instance Show PrimitiveDim where
  show (LitDim s) = T.unpack s
  show (PolyDim s) = "prim<" ++ T.unpack s ++ ">"

instance Show Dimension where
  show (NormDim dim) =
    if Map.size dim == 0
      then "dimensionless"
      else unwords $ map (\(name, amount) -> if amount == 1 then show name else show name ++ show amount) (List.sortOn (negate . snd) (Map.toList dim))
  show (PowDim dim) =
    if Map.size dim == 1
      then "^" ++ show (NormDim dim)
      else "^(" ++ show (NormDim dim) ++ ")"

instance Show Type where
  show (BaseDim dim) = show dim
  show (ListType dim) =
    "[" ++ show dim ++ "]"
  show (DictType dim) =
    "{" ++ List.intercalate "," (map (\(key, value) -> T.unpack key ++ ":" ++ show value) (Map.toAscList dim)) ++ "}"
  show (PolyDictType dim) =
    "{|" ++ List.intercalate "," (map (\(key, value) -> T.unpack key ++ ":" ++ show value) (Map.toAscList dim)) ++ "}"
  show (FuncType dimArg dimVal) =
    show dimArg ++ "->" ++ show dimVal
  show (PolyType a) =
    "type<" ++ T.unpack a ++ ">"

-- | Base Dimension returns the underlying normal dimension for lists. This
--   is used to check whether a dimension can be multiplied or added
baseDimension :: Type -> Type
baseDimension (ListType a) = baseDimension a
baseDimension x = x

-- | Multiplies two dimensions together
typeMult :: Type -> Type -> Either String Type
typeMult (BaseDim (NormDim a)) (BaseDim (NormDim b)) = Right . BaseDim . NormDim $ Map.filter (/= 0) $ Map.unionWith (+) a b
typeMult (ListType a) (ListType b) = ListType <$> typeMult a b
typeMult (ListType a) b = ListType <$> typeMult a b
typeMult a (ListType b) = ListType <$> typeMult a b
typeMult x y = Left $ "Cannot multiply " ++ show x ++ " to " ++ show y

dimensionless :: Type
dimensionless = BaseDim $ NormDim Map.empty

baseUnitPrim :: PrimitiveDim -> Maybe T.Text
baseUnitPrim (LitDim x) = Just x
baseUnitPrim (PolyDim _) = Nothing

baseUnitsDim :: Dimension -> Set.Set T.Text
baseUnitsDim (NormDim a) = Set.fromList . Maybe.mapMaybe baseUnitPrim $ Map.keys a
baseUnitsDim (PowDim a) = Set.fromList . Maybe.mapMaybe baseUnitPrim $ Map.keys a

-- | Base Units. Which units make up the type. Used for checking whether
--   units have been declared
baseUnits :: Type -> Set.Set T.Text
baseUnits (BaseDim a) = baseUnitsDim a
baseUnits (ListType a) = baseUnits a
baseUnits (DictType a) = Set.unions (map baseUnits $ Map.elems a)
baseUnits (PolyDictType a) = Set.unions (map baseUnits $ Map.elems a)
baseUnits (FuncType a b) = baseUnits a `Set.union` baseUnits b
baseUnits (PolyType _) = Set.empty

instance Show NumericValue where
  show (NumberValue val) = show val
  show (ListValue val) = "[" ++ List.intercalate ", " (map show val) ++ "]"
  show (DictValue val) = "{" ++ List.intercalate ", " (map (\(key, value) -> T.unpack key ++ "=" ++ show value) (Map.toAscList val)) ++ "}"
  show (FuncValue arg expr) = T.unpack arg ++ " -> " ++ show expr

data ExecutionValue
  = ExecutionValueNumber Double
  | ExecutionValueEmptyList
  | ExecutionValueDict (Map.Map T.Text ExecutionExpression)
  | ExecutionValueFunc T.Text ExecutionExpression
  deriving (Show)

data ExecutionExpression
  = EBinOp T.Text ExecutionExpression ExecutionExpression
  | EVariable T.Text
  | EAccess ExecutionExpression T.Text
  | EConstant ExecutionValue
  | ENegate ExecutionExpression
  deriving (Show)

data Operation = Add | Sub | Mult | Div | App | Power
  deriving (Show)

data PedantParseError = PedantParseError
  { ppeErrString :: T.Text,
    ppeColumn :: Int,
    ppeRow :: Int,
    ppeEndColumn :: Int,
    ppeEndRow :: Int,
    ppePrint :: T.Text
  }

data Scheme = Scheme [T.Text] Type

instance Show Scheme where
  show (Scheme [] t) = show t
  show (Scheme args t) = "∀" ++ T.unpack (T.unwords args) ++ ". " ++ show t

instance PrettyPrint PrimitiveDim where
  pPrint (LitDim s) = s
  pPrint (PolyDim s) = "'" <> s

instance PrettyPrint Dimension where
  pPrint (NormDim dim) =
    if Map.empty == dim
      then "1"
      else T.unwords $ map (\(name, amount) -> if amount == 1 then pPrint name else pPrint name <> T.pack (show amount)) (List.sortOn (negate . snd) (Map.toList dim))
  pPrint (PowDim dim) =
    if Map.empty == dim
      then "1"
      else ("^" <>) $ T.unwords $ map (\(name, amount) -> if amount == 1 then pPrint name else pPrint name <> T.pack (show amount)) (List.sortOn (negate . snd) (Map.toList dim))

instance PrettyPrint Type where
  pPrint (BaseDim s) = pPrint s
  pPrint (DictType d) =
    "{" <> T.intercalate "," (map (\(key, value) -> key <> ":" <> pPrint value) (Map.toAscList d)) <> "}"
  pPrint (ListType s) = "[" <> pPrint s <> "]"
  pPrint (PolyDictType d) =
    "{|" <> T.intercalate "," (map (\(key, value) -> key <> ":" <> pPrint value) (Map.toAscList d)) <> "}"
  pPrint (PolyType s) = "''" <> s
  pPrint (FuncType x y) =
    pPrint x <> " -> " <> pPrint y

instance PrettyPrint Scheme where
  pPrint (Scheme vars t) =
    let typeNames = imap (\i v -> (v, PolyType (T.pack [Char.chr (Char.ord 'a' + i)]))) vars
        dimNames = imap (\i v -> (v, NormDim $ Map.singleton (PolyDim (T.pack [Char.chr (Char.ord 'a' + i)])) 1)) vars
        sub = Substitution {subTypes = Map.fromList typeNames, subDimensions = Map.fromList dimNames}
     in pPrint $ apply sub t

class Types a where
  ftv :: a -> Set.Set T.Text
  apply :: Substitution -> a -> a

instance Types Dimension where
  ftv (NormDim n) =
    let keys = Map.keys n
     in Set.fromList $ Maybe.mapMaybe polymorphicVar keys
    where
      polymorphicVar :: PrimitiveDim -> Maybe T.Text
      polymorphicVar (PolyDim a) = Just a
      polymorphicVar _ = Nothing
  ftv (PowDim n) = ftv (NormDim n)

  apply s dim =
    case dim of
      NormDim n -> NormDim $ Map.foldlWithKey applyOne Map.empty n
      PowDim n -> PowDim $ Map.foldlWithKey applyOne Map.empty n
    where
      applyOne :: Map.Map PrimitiveDim Int -> PrimitiveDim -> Int -> Map.Map PrimitiveDim Int
      applyOne dimMap (LitDim x) power = Map.filter (/= 0) $ Map.unionWith (+) dimMap (Map.singleton (LitDim x) power)
      applyOne dimMap (PolyDim x) power =
        case Map.lookup x (subDimensions s) of
          Just (NormDim substitution) -> combine dimMap (Map.map (* power) substitution)
          Just (PowDim substitution) -> combine dimMap (Map.map (* power) substitution)
          Nothing -> combine dimMap (Map.singleton (PolyDim x) power)

      combine :: Map.Map PrimitiveDim Int -> Map.Map PrimitiveDim Int -> Map.Map PrimitiveDim Int
      combine a b = Map.filter (/= 0) $ Map.unionWith (+) a b

instance Types Type where
  ftv (PolyType n) = Set.singleton n
  ftv (FuncType x y) = ftv x `Set.union` ftv y
  ftv (DictType x) = Set.unions . map ftv $ Map.elems x
  ftv (ListType x) = ftv x
  ftv (BaseDim x) = ftv x
  ftv (PolyDictType x) = Set.unions . map ftv $ Map.elems x

  apply s (PolyType n) =
    case Map.lookup n (subTypes s) of
      Nothing -> PolyType n
      Just x -> apply s x
  apply s (FuncType x y) = FuncType (apply s x) (apply s y)
  apply s (BaseDim n) =
    BaseDim $ apply s n
  apply s (ListType n) =
    ListType $ apply s n
  apply s (PolyDictType n) =
    PolyDictType (Map.map (apply s) n)
  apply s (DictType n) =
    DictType (Map.map (apply s) n)

instance Types Scheme where
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr deleteFromSub s vars) t)
    where
      deleteFromSub :: T.Text -> Substitution -> Substitution
      deleteFromSub key sub =
        Substitution
          { subDimensions = Map.delete key (subDimensions sub),
            subTypes = Map.delete key (subTypes sub)
          }

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv l = Set.unions $ map ftv l

-- | There are two different types of substitutions. Dimensional and type substitutions.
data Substitution = Substitution
  { subTypes :: Map.Map T.Text Type,
    subDimensions :: Map.Map T.Text Dimension
  }
  deriving (Show)

imap :: (Int -> a -> b) -> [a] -> [b]
imap f list = _imap list 0
  where
    _imap (x : rest) idx =
      f idx x : _imap rest (idx + 1)
    _imap [] _ = []
