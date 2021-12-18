{-# LANGUAGE OverloadedStrings #-}

module Pedant.InBuilt
  ( inBuiltBinaryOperations,
    inBuiltPrefixOperations,
    Operation (..),
    Function (..),
    inBuiltFunctions,
  )
where

import qualified Data.Map as Map
import qualified Data.Text as T
import Pedant.Types
  ( Dimension (..),
    PrimitiveDim (..),
    Scheme (..),
    Type (..),
  )

data Operation = Operation
  { opName :: T.Text,
    opType :: Scheme,
    opPrecedence :: Int
  }

inBuiltBinaryOperations :: [Operation]
inBuiltBinaryOperations =
  [ Operation
      "+"
      (Scheme ["a", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "a" `FuncType` normalDimPoly "a"))
      4,
    Operation
      "-"
      (Scheme ["a", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "a" `FuncType` normalDimPoly "a"))
      4,
    Operation "*" (Scheme ["a", "b", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` normalDim [(PolyDim "a", 1), (PolyDim "b", 1)])) 3,
    Operation "/" (Scheme ["a", "b", "t"] $ normalDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` normalDim [(PolyDim "a", 1), (PolyDim "b", -1)])) 3,
    Operation "^" (Scheme ["a", "b", "t"] $ powerDimPoly "a" `FuncType` (normalDimPoly "b" `FuncType` powerDim [(PolyDim "a", 1), (PolyDim "b", 1)])) 1,
    Operation ":" (Scheme ["a"] $ PolyType "a" `FuncType` (ListType (PolyType "a") `FuncType` ListType (PolyType "a"))) 4,
    Operation "" (Scheme ["a", "b", "t"] $ ((PolyType "a" `FuncType` PolyType "b") `FuncType` PolyType "a") `FuncType` PolyType "b") 0
  ]

inBuiltPrefixOperations :: [Operation]
inBuiltPrefixOperations = [Operation "-" (Scheme ["a", "t"] $ normalDimPoly "a" `FuncType` normalDimPoly "a") 2]

data Function = Function
  { funcName :: T.Text,
    funcType :: Scheme
  }

inBuiltFunctions :: [Function]
inBuiltFunctions = [Function "ln" (Scheme ["t", "a"] $ powerDimPoly "a" `FuncType` normalDimPoly "a")]

normalDimPoly :: T.Text -> Type
normalDimPoly name = BaseDim $ NormDim $ Map.singleton (PolyDim name) 1

powerDimPoly :: T.Text -> Type
powerDimPoly name = BaseDim . PowDim $ Map.singleton (PolyDim name) 1

normalDim :: [(PrimitiveDim, Int)] -> Type
normalDim powers = BaseDim . NormDim $ Map.fromList powers

powerDim :: [(PrimitiveDim, Int)] -> Type
powerDim powers = BaseDim . PowDim $ Map.fromList powers
