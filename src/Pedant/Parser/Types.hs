{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module Pedant.Parser.Types (Assignment(..), Statement(..), PositionData(..), Positioned(..), Expression(..)) where

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Pedant.Types as Types

data Assignment = Assignment
  { assignmentName :: T.Text,
    assignmentArguments :: [T.Text],
    assignmentExpression :: Positioned Expression
  }
  deriving (Show)

data Statement
  = AssignmentStatement Assignment
  | UnitStatement [Positioned T.Text]
  | ImportStatement (Positioned T.Text) [Positioned T.Text]
  deriving (Show)

data PositionData = PositionData {
  pdOffset :: Int,
  pdLength :: Int
}
  deriving (Show, Eq, Ord)

data Positioned a = Positioned {
  positionedData :: PositionData,
  positionedValue :: a
}  deriving (Show)

instance Eq a => Eq (Positioned a) where
  (==) (Positioned a1 b1) (Positioned a2 b2) = a1 == a2 && b1 == b2

instance Functor Positioned where
  fmap f (Positioned p x) = Positioned p (f x)

data DimensionPart = DimensionPart
  { pdpName :: T.Text,
    pdpPower :: Int
  }
  deriving (Show, Eq)

instance Types.PrettyPrint DimensionPart where
  pPrint (DimensionPart name power) =
    if power == 1
      then name
      else T.concat [name, T.pack (show power)]

data Dimension
  = PowParseDim [Positioned DimensionPart]
  | NormalParseDim [Positioned DimensionPart]
  deriving (Show, Eq)

instance Types.PrettyPrint Dimension where
  pPrint :: Dimension -> T.Text
  pPrint (NormalParseDim parts) = T.unwords (map Types.pPrint parts)
  pPrint (PowParseDim parts) = "^" <> T.unwords (map Types.pPrint parts)


data Expression
  = BinOp Types.BinaryOperation (Positioned Expression) (Positioned Expression)
  | Variable Types.VariableName
  | Number Double Dimension
  | List [Positioned Expression]
  | Let Types.VariableName Expression Expression
  | Abs Types.VariableName Expression
  | Record (Map.Map Types.RecordKey (Positioned Expression))
  | Prefix Types.PrefixOperation (Positioned Expression)
  | Access (Positioned Expression) Types.AccessKey
  deriving (Show, Eq)

instance Types.PrettyPrint a => Types.PrettyPrint (Positioned a) where
  pPrint (Positioned _ a) = Types.pPrint a

instance Types.PrettyPrint Expression  where
  pPrint (BinOp (Types.BinaryOperation op) e1 e2) = T.unwords [Types.pPrint e1, op, Types.pPrint e2]
  pPrint (Variable (Types.VariableName var)) = var
  pPrint (Prefix (Types.PrefixOperation op) e1) = T.concat [op, Types.pPrint e1]
  pPrint (Access e1 (Types.AccessKey att)) = T.concat [Types.pPrint e1, att]
  pPrint (Number num dim) = T.unwords [T.pack $ show num, Types.pPrint dim]
  pPrint (List list) = T.concat ["[", T.intercalate ", " (map Types.pPrint list), "]"]
  pPrint (Let (Types.VariableName name) value expr) = T.concat ["let ", name, " = ", Types.pPrint value, " in ", Types.pPrint expr]
  pPrint (Abs (Types.VariableName name) value) = T.concat ["\\", name, " -> ", Types.pPrint value]
  pPrint (Record op) =
    T.concat
      [ "{",
        T.intercalate ", " (map (\(Types.RecordKey key, value) -> T.concat [key, " = ", Types.pPrint value]) (Map.toAscList op)),
        "}"
      ]

data Function = NaturalLogarithm
  deriving (Show)