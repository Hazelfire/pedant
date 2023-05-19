{-# LANGUAGE OverloadedStrings #-}

module Pedant.Types
  ( Operation (..),
    ExecutionExpression (..),
    ExecutionStatement (..),
    PedantParseError (..),
    ExecutionValue (..),
    NumericValue (..),
    PrettyPrint (..),
    InternalFunction (..),
    BinaryOperation(..),
    VariableName(..),
    UnitName(..),
    RecordKey(..),
    PrefixOperation(..),
  AccessKey(..)
  )
where

import qualified Data.List as List
import qualified Data.Map as Map
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
  | InternalFunctionValue InternalFunction

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
    InternalFunctionValue x -> InternalFunctionValue x

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


instance Show NumericValue where
  show (NumberValue val) = show val
  show (ListValue val) = "[" ++ List.intercalate ", " (map show val) ++ "]"
  show (DictValue val) = "{" ++ List.intercalate ", " (map (\(key, value) -> T.unpack key ++ "=" ++ show value) (Map.toAscList val)) ++ "}"
  show (FuncValue arg expr) = T.unpack arg ++ " -> " ++ show expr
  show (InternalFunctionValue _) = "INTERNAL FUNCTION"

data ExecutionValue
  = ExecutionValueNumber Double
  | ExecutionValueList [ExecutionExpression]
  | ExecutionValueDict (Map.Map T.Text ExecutionExpression)
  | ExecutionValueFunc T.Text ExecutionExpression
  deriving (Show)

newtype BinaryOperation = BinaryOperation T.Text deriving (Show, Eq)
newtype VariableName = VariableName T.Text deriving (Show, Eq, Ord)
newtype UnitName = UnitName T.Text deriving (Show, Eq, Ord)
newtype RecordKey = RecordKey T.Text deriving (Show, Eq, Ord)
newtype PrefixOperation = PrefixOperation T.Text deriving (Show, Eq)
newtype AccessKey = AccessKey T.Text deriving (Show, Eq)

data ExecutionExpression
  = EBinOp T.Text ExecutionExpression ExecutionExpression
  | EVariable T.Text
  | EAccess ExecutionExpression T.Text
  | EConstant ExecutionValue
  | ENegate ExecutionExpression
  | EInternalFunc InternalFunction
  deriving (Show)

newtype InternalFunction = InternalFunction (NumericValue -> NumericValue)

instance Show InternalFunction where
  show _ = "INTERNAL FUNCTION"

data ExecutionStatement
  = ExecAssignment T.Text ExecutionExpression
  | ExecImport T.Text (Set.Set T.Text)
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
  deriving (Show)
