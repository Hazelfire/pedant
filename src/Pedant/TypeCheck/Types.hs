{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Pedant.TypeCheck.Types
  ( PolyTypeName (..),
    Scheme (..),
    Substitution (..),
    PrimitiveDim(..),
    Dimension(..),
    NormalDimension(..),
    Type (..),
    TypeEnv (..),
    TypeInferenceMonad (..),
    newPolyType,
    newPolyDim,
    multiplyNormalDimensions,
    newPolyNormDim
  )
where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Ordered as OMap
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Pedant.Parser.Types as Parser
import qualified Pedant.Types as Types

newtype PolyTypeName = PolyTypeName {showTypeNameNumber :: Int} deriving (Show, Eq, Ord)

data Scheme = Scheme [PolyTypeName] Type deriving (Show)

data TypeCheckResult = TypeCheckResult Type Types.ExecutionExpression

type UnificationTrace = [(Type, Type)]

newtype TIState = TIState {tiSeed :: Int}

-- | Unification Monad
type UM a = Except.ExceptT UnificationTrace (State.State TIState) a

data VariableInfo = VariableInfo
  { variableInfoScheme :: Scheme,
    variableInfoExecutionExpression :: Types.ExecutionExpression,
    variableInfoParserStatement :: Parser.Assignment
  }
  deriving (Show)

newtype TypeEnv = TypeEnv {teVarMap :: OMap.OMap Types.VariableName VariableInfo}
  deriving (Show)

-- | The state of the type checker
data TypeCheckState = TypeCheckState
  { -- | The environment of the checker. This contains references to all the variables and schemes of those variables currently declared.
    tcsEnv :: TypeEnv,
    -- | Substitutions, the current substitutions that are required for the expression to unify
    tcsSubs :: Substitution,
    -- | Units, the units currently declared
    tcsUnits :: Set.Set Types.UnitName,
    -- | A list of the modules that have been checked
    tcsCheckedModules :: Set.Set T.Text,
    tcsCurrentModule :: T.Text,
    tcsSeed :: Int
  }

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (map (variableInfoScheme . snd) $ OMap.assocs env)
  apply s (TypeEnv env) = TypeEnv (fmap (\vi -> vi {variableInfoScheme = apply s (variableInfoScheme vi)}) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where
    vars = Set.toList (ftv t `Set.difference` ftv env)

addToEnv :: Types.VariableName -> VariableInfo -> TypeEnv -> TypeEnv
addToEnv key var (TypeEnv env) = TypeEnv ((key, var) OMap.|< env)

newtype TypeInferenceMonad a = TypeInferenceMonad {runTI :: Except.ExceptT (Parser.Positioned TypeError) (State.State TypeCheckState) a}

newPolyNormDim :: TypeInferenceMonad NormalDimension
newPolyNormDim = TypeInferenceMonad $ State.gets (polyNormDim . PolyTypeName . tcsSeed)

newPolyType :: TypeInferenceMonad Type
newPolyType = TypeInferenceMonad $ State.gets (PolyType . PolyTypeName . tcsSeed)

newPolyDim :: TypeInferenceMonad Dimension
newPolyDim = TypeInferenceMonad $ State.gets (PolyDim . PolyTypeName . tcsSeed)

imap :: (Int -> a -> b) -> [a] -> [b]
imap f list = _imap list 0
  where
    _imap (x : rest) idx =
      f idx x : _imap rest (idx + 1)
    _imap [] _ = []

instance Types.PrettyPrint Scheme where
  pPrint (Scheme vars t) =
    let typeNames = imap (\i v -> (v, PolyType (PolyTypeName i))) vars
        dimNames = imap (\i v -> (v, PolyDim (PolyTypeName i))) vars
        sub = Substitution {subTypes = Map.fromList typeNames, subDimensions = Map.fromList dimNames}
     in Types.pPrint $ apply sub t

class Types a where
  ftv :: a -> Set.Set PolyTypeName
  apply :: Substitution -> a -> a


instance Types Type where
  ftv (PolyType n) = Set.singleton n
  ftv (FuncType x y) = ftv x `Set.union` ftv y
  ftv (DictType x) = Set.unions . map ftv $ Map.elems x
  ftv (ListType x) = ftv x
  ftv (NumberType x) = ftv x
  ftv (PolyDictType x) = Set.unions . map ftv $ Map.elems x

  apply s (PolyType n) =
    case Map.lookup n (subTypes s) of
      Nothing -> PolyType n
      Just x -> apply s x
  apply s (FuncType x y) = FuncType (apply s x) (apply s y)
  apply s (NumberType n) =
    NumberType $ apply s n
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
      deleteFromSub :: PolyTypeName -> Substitution -> Substitution
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
  { subTypes :: Map.Map PolyTypeName Type,
    subDimensions :: Map.Map PolyTypeName Dimension
  }
  deriving (Show)


data PrimitiveDim
  = -- | Literal dimension, such as years
    LitDim Types.UnitName
  | -- | Polymorphic dimension, such as <a>
    PolyPrimDim PolyTypeName
  deriving (Eq, Ord)

newtype NormalDimension = NormalDimension {normDimComponents :: Map.Map PrimitiveDim Int } deriving (Eq, Ord)
-- | Represents a dimension! This is the big thing in pedant
data Dimension
  = -- | a non-power dimension (such as years)
    NormDim NormalDimension
  | -- | A power dimension (such as ^years-1)
    PowDim (Map.Map PrimitiveDim Int)
  | PolyDim PolyTypeName
  deriving (Eq)

dimensionless :: Dimension
dimensionless = NormDim (NormalDimension Map.empty)

polyNormDim :: PolyTypeName -> NormalDimension
polyNormDim name = NormalDimension (Map.singleton (PolyPrimDim name) 1)

multiplyNormalDimensions :: NormalDimension -> NormalDimension -> NormalDimension
multiplyNormalDimensions (NormalDimension map1) (NormalDimension map2) = NormalDimension $ Map.filter (/= 0) $ Map.unionWith (+) map1 map2

data Type
  = -- | An actual dimension, such as people years-1
    NumberType Dimension
  | -- | A list of a dimension, such as [years]
    ListType Type
  | -- | A dictionary of dimensions, such as {x:meters,y:meters}
    DictType (Map.Map T.Text Type)
  | -- | A polymorphic dictionary (a dictionary that contains these keys or more). Such as {|x:meters,y:meters}
    PolyDictType (Map.Map T.Text Type)
  | -- | A Function. Such as years -> meters
    FuncType Type Type
  | -- | A Polymorphic Type. A type that could be anything
    PolyType PolyTypeName
  deriving (Eq)

infixr 7 `FuncType`


-- Show instances for dimensions. Shows internal details and may be
-- difficult to read.
instance Show PrimitiveDim where
  show (LitDim s) = show s
  show (PolyPrimDim s) = "prim<" ++ show s ++ ">"

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
  show (NumberType dim) = show dim
  show (ListType dim) =
    "[" ++ show dim ++ "]"
  show (DictType dim) =
    "{" ++ List.intercalate "," (map (\(key, value) -> T.unpack key ++ ":" ++ show value) (Map.toAscList dim)) ++ "}"
  show (PolyDictType dim) =
    "{|" ++ List.intercalate "," (map (\(key, value) -> T.unpack key ++ ":" ++ show value) (Map.toAscList dim)) ++ "}"
  show (FuncType dimArg dimVal) =
    show dimArg ++ "->" ++ show dimVal
  show (PolyType a) =
    "type<" ++ show a ++ ">"

-- | Base Dimension returns the underlying normal dimension for lists. This
--   is used to check whether a dimension can be multiplied or added
baseDimension :: Type -> Type
baseDimension (ListType a) = baseDimension a
baseDimension x = x

-- | Multiplies two dimensions together
typeMult :: Type -> Type -> Either String Type
typeMult (NumberType (NormDim a)) (NumberType (NormDim b)) = Right . NumberType . NormDim $ Map.filter (/= 0) $ Map.unionWith (+) a b
typeMult (ListType a) (ListType b) = ListType <$> typeMult a b
typeMult (ListType a) b = ListType <$> typeMult a b
typeMult a (ListType b) = ListType <$> typeMult a b
typeMult x y = Left $ "Cannot multiply " ++ show x ++ " to " ++ show y


baseUnitPrim :: PrimitiveDim -> Maybe Types.UnitName
baseUnitPrim (LitDim x) = Just x
baseUnitPrim (PolyPrimDim _) = Nothing

baseUnitsDim :: Dimension -> Set.Set Types.UnitName
baseUnitsDim (NormDim a) = Set.fromList . Maybe.mapMaybe baseUnitPrim $ Map.keys a
baseUnitsDim (PowDim a) = Set.fromList . Maybe.mapMaybe baseUnitPrim $ Map.keys a

-- | Base Units. Which units make up the type. Used for checking whether
--   units have been declared
baseUnits :: Type -> Set.Set Types.UnitName
baseUnits (NumberType a) = baseUnitsDim a
baseUnits (ListType a) = baseUnits a
baseUnits (DictType a) = Set.unions (map baseUnits $ Map.elems a)
baseUnits (PolyDictType a) = Set.unions (map baseUnits $ Map.elems a)
baseUnits (FuncType a b) = baseUnits a `Set.union` baseUnits b
baseUnits (PolyType _) = Set.empty

instance Types.PrettyPrint PrimitiveDim where
  pPrint (LitDim (Types.UnitName s)) = s
  pPrint (PolyPrimDim (PolyTypeName s)) = "'" <> T.pack (show s)

instance Types.PrettyPrint Dimension where
  pPrint (NormDim dim) =
    if Map.empty == dim
      then "1"
      else T.unwords $ map (\(name, amount) -> if amount == 1 then Types.pPrint name else Types.pPrint name <> T.pack (show amount)) (List.sortOn (negate . snd) (Map.toList dim))
  pPrint (PowDim dim) =
    if Map.empty == dim
      then "1"
      else "^" <> Types.pPrint (NormDim dim)

instance Types.PrettyPrint Type where
  pPrint (NumberType s) = Types.pPrint s
  pPrint (DictType d) =
    "{" <> T.intercalate "," (map (\(key, value) -> key <> ":" <> Types.pPrint value) (Map.toAscList d)) <> "}"
  pPrint (ListType s) = "[" <> Types.pPrint s <> "]"
  pPrint (PolyDictType d) =
    "{|" <> T.intercalate "," (map (\(key, value) -> key <> ":" <> Types.pPrint value) (Map.toAscList d)) <> "}"
  pPrint (PolyType (PolyTypeName s)) = "''" <> T.pack (show s)
  pPrint (FuncType x y) =
    Types.pPrint x <> " -> " <> Types.pPrint y

-- | A Type Error. Decribes a problem that occured during type checking
data TypeError
  = UnificationError ReasonForUnification UnificationTrace
  | MissingUnitError T.Text
  | MissingVariableError T.Text
  | MissingImportError T.Text T.Text
  | MissingModuleError T.Text
  | InternalError T.Text
  deriving (Eq)

data ReasonForUnification
  = BinaryOpUnificationReason T.Text (Parser.Positioned Parser.Expression, Type) (Parser.Positioned Parser.Expression, Type)
  | PrefixOpUnificationReason T.Text (Parser.Positioned Parser.Expression, Type)
  | AccessUnificationReason (Parser.Positioned Parser.Expression, Type) T.Text
  deriving (Eq)

typeErrorMessage :: TypeError -> T.Text
typeErrorMessage te =
  case te of
    UnificationError reason _ ->
      case reason of
        BinaryOpUnificationReason "+" (p1, t1) (p2, t2) ->
          T.concat
            [ "Can only add dimension that are the same.\n",
              Types.pPrint p1,
              " has the type ",
              Types.pPrint t1,
              " and ",
              Types.pPrint p2,
              " has the type ",
              Types.pPrint t2
            ]
        BinaryOpUnificationReason "-" (p1, t1) (p2, t2) ->
          T.concat
            [ "Can only subtract dimensions that are the same.\n",
              Types.pPrint p1,
              " has the type ",
              Types.pPrint t1,
              " and ",
              Types.pPrint p2,
              " has the type ",
              Types.pPrint t2
            ]
        BinaryOpUnificationReason op (p1, t1) (p2, t2) ->
          T.concat
            [ op,
              " must be called on a number.\n",
              Types.pPrint p1,
              " has the type ",
              Types.pPrint t1,
              " and ",
              Types.pPrint p2,
              " has the type ",
              Types.pPrint t2
            ]
        PrefixOpUnificationReason op (p1, t1) ->
          T.concat
            [ op,
              " must be called on a number.\n",
              Types.pPrint p1,
              " has the type ",
              Types.pPrint t1
            ]
        AccessUnificationReason (p1, t1) key ->
          T.concat
            [ Types.pPrint p1,
              " has type ",
              Types.pPrint t1,
              " does not have the key ",
              key
            ]
    (MissingUnitError unitName) ->
      T.concat
        [ "unit ",
          unitName,
          " not declared. Try adding a \"unit ",
          unitName,
          "\" statement before this line"
        ]
    (MissingVariableError varName) ->
      T.concat
        [ "variable ",
          varName,
          " not declared."
        ]
    InternalError err ->
      T.append "INTERNAL ERROR. YOU SHOULD NOT BE GETTING THIS: " err
    MissingImportError moduleName variable ->
      T.concat
        [ "Could not find name ",
          variable,
          " in module ",
          moduleName,
          "."
        ]
    (MissingModuleError moduleName) ->
      T.concat
        [ "Could not find module ",
          moduleName,
          "."
        ]

errorComponentLen (Parser.Positioned (Parser.PositionData _ l) _) = l