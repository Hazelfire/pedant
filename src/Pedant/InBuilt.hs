{-# LANGUAGE OverloadedStrings #-}

module Pedant.InBuilt
  ( inBuiltBinaryOperations,
    inBuiltPrefixOperations,
    Operation (..),
    Function (..),
    OpFunc (..),
    inBuiltFunctions,
  )
where

import qualified Data.Map as Map
import qualified Data.Text as T
import Pedant.Types
  ( Dimension (..),
    InternalFunction (..),
    NumericValue (..),
    PrimitiveDim (..),
    Scheme (..),
    Type (..),
  )

data Operation = Operation
  { opName :: T.Text,
    opType :: Scheme,
    opPrecedence :: Int,
    opFunc :: OpFunc
  }

data OpFunc
  = BinFunc (NumericValue -> NumericValue -> NumericValue)
  | UnaryFunc (NumericValue -> NumericValue)

inBuiltBinaryOperations :: [Operation]
inBuiltBinaryOperations =
  [ Operation
      "+"
      (Scheme ["a", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "a" `FuncType` normalDimPoly "a"))
      4
      (BinFunc (+)),
    Operation
      "-"
      (Scheme ["a", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "a" `FuncType` normalDimPoly "a"))
      4
      (BinFunc (-)),
    Operation
      "*"
      (Scheme ["a", "b", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` normalDim [(PolyDim "a", 1), (PolyDim "b", 1)]))
      3
      (BinFunc (*)),
    Operation
      "/"
      (Scheme ["a", "b", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` normalDim [(PolyDim "a", 1), (PolyDim "b", -1)]))
      3
      (BinFunc (/)),
    Operation
      "^"
      (Scheme ["a", "b", "t"] $ powerDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` powerDim [(PolyDim "a", 1), (PolyDim "b", 1)]))
      1
      (BinFunc (**)),
    Operation
      ":"
      (Scheme ["a"] $ PolyType "a" `FuncType` (ListType (PolyType "a") `FuncType` ListType (PolyType "a")))
      4
      (BinFunc append),
    Operation "" (Scheme ["a", "b", "t"] $ ((PolyType "a" `FuncType` PolyType "b") `FuncType` PolyType "a") `FuncType` PolyType "b") 0 (BinFunc (const id))
  ]

append :: NumericValue -> NumericValue -> NumericValue
append a (ListValue b) = ListValue (a : b)
append _ b = b

inBuiltPrefixOperations :: [Operation]
inBuiltPrefixOperations = [Operation "-" (Scheme ["a", "t"] $ normalDimPoly "a" `FuncType` normalDimPoly "a") 2 (UnaryFunc negate)]

data Function = Function
  { funcName :: T.Text,
    funcType :: Scheme,
    funcDef :: InternalFunction
  }

inBuiltFunctions :: [Function]
inBuiltFunctions = [Function "ln" (Scheme ["t", "a"] $ powerDimPoly "a" `FuncType` normalDimPoly "a") (InternalFunction log)]

normalDimPoly :: T.Text -> Type
normalDimPoly name = BaseDim $ NormDim $ Map.singleton (PolyDim name) 1

powerDimPoly :: T.Text -> Type
powerDimPoly name = BaseDim . PowDim $ Map.singleton (PolyDim name) 1

normalDim :: [(PrimitiveDim, Int)] -> Type
normalDim powers = BaseDim . NormDim $ Map.fromList powers

powerDim :: [(PrimitiveDim, Int)] -> Type
powerDim powers = BaseDim . PowDim $ Map.fromList powers
