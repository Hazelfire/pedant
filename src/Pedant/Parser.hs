{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The parser for pedant. Creates a syntax tree from the file
module Pedant.Parser
  ( parseProgram,
    PedantParseError (..),
    Positioned (..),
    Operation (..),
    DimensionPart (..),
    Dimension (..),
    PositionData (..),
    Expression (..),
    Assignment (..),
    Statement (..),
    makeErrorBundle,
  )
where

import qualified Control.Monad.Combinators.Expr as Expr
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Pedant.InBuilt as InBuilt
import Pedant.Types (Operation (..), PedantParseError (..), PrettyPrint (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | We now define a parser for this typed language
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

instance PrettyPrint DimensionPart where
  pPrint (DimensionPart name power) =
    if power == 1
      then name
      else T.concat [name, T.pack (show power)]

data Dimension
  = PowParseDim [Positioned DimensionPart]
  | NormalParseDim [Positioned DimensionPart]
  deriving (Show, Eq)

instance PrettyPrint Dimension where
  pPrint (NormalParseDim parts) = T.unwords (map pPrint parts)
  pPrint (PowParseDim parts) = "^" <> T.unwords (map pPrint parts)

newtype BinaryOperation = BinaryOperation T.Text deriving (Show, Eq)
newtype VariableName = VariableName T.Text deriving (Show, Eq)
newtype RecordKey = RecordKey T.Text deriving (Show, Eq, Ord)
newtype PrefixOperation = PrefixOperation T.Text deriving (Show, Eq)
newtype AccessKey = AccessKey T.Text deriving (Show, Eq)

data Expression
  = BinOp BinaryOperation (Positioned Expression) (Positioned Expression)
  | Variable VariableName
  | Number Double Dimension
  | List [Positioned Expression]
  | Record (Map.Map RecordKey (Positioned Expression))
  | Prefix PrefixOperation (Positioned Expression)
  | Access (Positioned Expression) AccessKey
  deriving (Show, Eq)

instance PrettyPrint a => PrettyPrint (Positioned a) where
  pPrint (Positioned _ a) = pPrint a

instance PrettyPrint Expression  where
  pPrint (BinOp (BinaryOperation op) e1 e2) = T.unwords [pPrint e1, op, pPrint e2]
  pPrint (Variable (VariableName var)) = var
  pPrint (Prefix (PrefixOperation op) e1) = T.concat [op, pPrint e1]
  pPrint (Access e1 (AccessKey att)) = T.concat [pPrint e1, att]
  pPrint (Number num dim) = T.unwords [T.pack $ show num, pPrint dim]
  pPrint (List list) = T.concat ["[", T.intercalate ", " (map pPrint list), "]"]
  pPrint (Record op) =
    T.concat
      [ "{",
        T.intercalate ", " (map (\(RecordKey key, value) -> T.concat [key, " = ", pPrint value]) (Map.toAscList op)),
        "}"
      ]

data Function = NaturalLogarithm
  deriving (Show)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    hspace1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

scn :: Parser ()
scn =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Parentheses, brackets, and braces
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- Comma-separated list of expressions
commaSepExpr :: Parser () -> Parser [Positioned Expression]
commaSepExpr sc' = lexeme (pExpr sc' `sepBy` symbol ",")

pVariable :: Parser (Positioned Expression)
pVariable = position (Variable . VariableName <$> identifier)

number :: RealFloat a => Parser a
number = lexeme (try L.float <|> L.decimal)

pTypedNumber :: Parser () -> Parser Expression
pTypedNumber sc' = Number <$> number <*> parseDimension sc'

-- Lists are parsed as an empty list with all elements concatenated into them
pTypedList :: Parser () -> Parser Expression
pTypedList sc' = List <$> brackets (commaSepExpr sc')

pTypedRecord :: Parser () -> Parser Expression
pTypedRecord sc' = Record . Map.fromList <$> braces (pair `sepBy` symbol ",")
  where
    pair :: Parser (RecordKey, Positioned Expression)
    pair = (,) <$> (RecordKey <$> identifier )<* symbol "=" <*> pExpr sc'

parseDimension :: Parser () -> Parser Dimension
parseDimension sc' = (PowParseDim <$> try (char '^' *> parseNextDim [])) <|> (NormalParseDim <$> pLoop [])
  where
    pLoop :: [Positioned DimensionPart] -> Parser [Positioned DimensionPart]
    pLoop p = parseNextDim p <|> return p

    parseNextDim :: [Positioned DimensionPart] -> Parser [Positioned DimensionPart]
    parseNextDim oldDim = do
      dim <- L.lexeme sc' (position pSingleDim)
      pLoop (dim : oldDim)

    pSingleDim :: Parser DimensionPart
    pSingleDim = do
      name <- (:) <$> letterChar <*> many letterChar
      number <- fromInteger <$> L.signed (pure ()) L.decimal <|> return 1
      return (DimensionPart (T.pack name) number)


identifier :: Parser T.Text
identifier = lexeme $ T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "name")

pTerm :: Parser () -> Parser (Positioned Expression)
pTerm sc' =
  let mainExpression =
        choice
          [ parens (pExpr sc'),
            L.lexeme sc' pVariable,
            position (pTypedList sc'),
            position (pTypedRecord sc'),
            position (pTypedNumber sc')
          ]
   in do
        expression <- mainExpression
        L.lexeme sc' (accessor expression <|> return expression)
  where
    accessor :: Positioned Expression -> Parser (Positioned Expression)
    accessor expr = do
      _ <- char '.'
      L.lexeme sc' $ position (Access expr <$> (AccessKey <$> identifier))

-- | Runs full programs, such as
-- >>> :set -XOverloadedStrings
-- >>> parse program "" "x = 2\n"
program :: Parser [Statement]
program = manyTill (pStatement <* scn) eof

-- | Runs full programs, such as
-- >>> :set -XOverloadedStrings
-- >>> parse pStatement "" "x = 2\n"
-- Right (AssignmentStatement (Assignment {assignmentName = "x", assignmentArguments = [], assignmentExpression = Positioned {positionedData = PositionData {pdOffset = 6, pdLength = 1}, positionedValue = PConstant (ParseNumber 2.0 (NormalParseDim []))}}))
pStatement :: Parser Statement
pStatement = L.lineFold scn $ \sc' ->
  let new_space_consumer = try sc' <|> sc
   in choice
        [ pImport new_space_consumer,
          pUnit new_space_consumer,
          AssignmentStatement <$> pAssignment new_space_consumer
        ]

-- | Import statements. They come in the form of:
-- import [module]([name1, name2, ...])
-- >>> :set -XOverloadedStrings
-- >>> parse (pImport sc) "" "import moduleName(name1, name2)"
-- Right (ImportStatement (Positioned (PositionData 7 10) "moduleName") [Positioned (PositionData 18 5) "name1",Positioned (PositionData 25 5) "name2"])
--
-- Empty imports are invalid:
-- >>> parse (pImport sc) "" "import moduleName()"
-- Left (ParseErrorBundle {bundleErrors = TrivialError 18 (Just (Tokens (')' :| ""))) (fromList [Label ('n' :| "ame")]) :| [], bundlePosState = PosState {pstateInput = "import moduleName()\n", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
pImport :: Parser () -> Parser Statement
pImport sc' = do
  _ <- try (L.lexeme sc' (symbol "import"))
  fileName <- position identifier
  _ <- L.lexeme sc' (symbol "(")
  ImportStatement fileName <$> pImportList
  where
    pImportList :: Parser [Positioned T.Text]
    pImportList = do
      importName <- position identifier
      choice
        [ do
            _ <- L.lexeme sc' (symbol ",")
            (importName :) <$> pImportList,
          do
            _ <- L.lexeme sc' (symbol ")")
            return [importName]
        ]

pUnit :: Parser () -> Parser Statement
pUnit sc' = do
  _ <- L.lexeme sc' (symbol "unit")
  UnitStatement <$> (position identifier `sepBy1` sc')

-- | Parses an assignment:
-- >>> :set -XOverloadedStrings
-- >>> parse (pAssignment sc) "" "x = 2"
pAssignment :: Parser () -> Parser Assignment
pAssignment sc' = Assignment <$> identifier <*> many identifier <* symbol "=" <*> pExpr sc'

pExpr :: Parser () -> Parser (Positioned Expression)
pExpr sc' =
  Expr.makeExprParser (pTerm sc') (operatorTable sc')

operatorTable :: Parser () -> [[Expr.Operator Parser (Positioned Expression)]]
operatorTable sc' =
  let prefixOps = map (\op -> (InBuilt.opPrecedence op, prefix sc' (InBuilt.opName op) (Prefix $ PrefixOperation (InBuilt.opName op)))) InBuilt.inBuiltPrefixOperations
      binaryOps = map (\op -> (InBuilt.opPrecedence op, binary sc' (InBuilt.opName op) (BinOp $ BinaryOperation (InBuilt.opName op)))) InBuilt.inBuiltBinaryOperations
      combinedOps = prefixOps ++ binaryOps
      sortedOps = List.groupBy (\a b -> fst a == fst b) (List.sortOn fst combinedOps)
   in map (map snd) sortedOps

position :: Parser a -> Parser (Positioned a)
position parser = do
  offset <- getOffset
  x <- parser
  newOffset <- getOffset
  return (Positioned (PositionData offset (newOffset - offset)) x)

binary :: Parser () -> Text -> (Positioned Expression -> Positioned Expression -> Expression) -> Expr.Operator Parser (Positioned Expression)
binary sc' name f =
  Expr.InfixL
    ( do
        _ <- L.symbol sc' name
        return (combinePositions f)
    )

combinePositions :: (Positioned a -> Positioned b -> c) -> Positioned a -> Positioned b -> Positioned c
combinePositions f a@(Positioned (PositionData startOffset _) _) b@(Positioned (PositionData start2 l) _) =
  Positioned (PositionData startOffset ((start2 + l) - startOffset)) (f a b)

prefix :: Parser () -> Text -> (Positioned Expression -> Expression) -> Expr.Operator Parser (Positioned Expression)
prefix sc' name f =
  Expr.Prefix
    ( do
        offset <- getOffset
        _ <- L.symbol sc' name
        return (\x@(Positioned (PositionData childOffset l) _) -> Positioned (PositionData offset ((childOffset + l) - offset)) (f x))
    )

errorBundleToPedantError :: ShowErrorComponent b => ParseErrorBundle Text b -> NonEmpty PedantParseError
errorBundleToPedantError bundle =
  let (SourcePos _ column row) = pstateSourcePos (bundlePosState bundle)
   in fmap
        ( \err ->
            PedantParseError
              (T.pack $ parseErrorTextPretty err)
              (unPos column - 1)
              (unPos row - 1)
              (unPos row - 1)
              (unPos column)
              (T.pack $ errorBundlePretty bundle)
        )
        (bundleErrors bundle)

parseProgram :: String -> T.Text -> Either (NonEmpty PedantParseError) [Statement]
parseProgram name contents = do
  Bifunctor.first errorBundleToPedantError $ parse program name (T.append contents "\n")

-- | Makes a bundle of errors based on a file name contents and positions.
makeErrorBundle :: (ShowErrorComponent a, a ~ Positioned b) => a -> String -> T.Text -> PedantParseError
makeErrorBundle err@(Positioned (PositionData offset l) _) filename contents =
  let initialPosState =
        PosState
          { pstateInput = contents,
            pstateOffset = 0,
            pstateSourcePos = initialPos filename,
            pstateTabWidth = defaultTabWidth,
            pstateLinePrefix = ""
          }
      ([(_, sourcePos), (_, endSourcePos)], _) = attachSourcePos id [offset, offset + l] initialPosState
      newPosState = initialPosState {pstateInput = contents, pstateOffset = 0}
      errorBundle = ParseErrorBundle (FancyError offset (Set.singleton (ErrorCustom err)) :| []) newPosState
   in PedantParseError
        { ppeErrString = T.pack $ showErrorComponent err,
          ppeColumn = unPos (sourceColumn sourcePos) - 1,
          ppeRow = unPos (sourceLine sourcePos) - 1,
          ppeEndColumn = unPos (sourceColumn endSourcePos) - 1,
          ppeEndRow = unPos (sourceLine endSourcePos) - 1,
          ppePrint = T.pack $ errorBundlePretty errorBundle
        }
