{-# LANGUAGE OverloadedStrings #-}

-- | The parser for pedant. Creates a syntax tree from the file
module Parser
  ( parseProgram,
    PedantParseError (..),
    Positioned (..),
    Operation (..),
    ParseDimensionPart (..),
    ParseDimension (..),
    ParseLiteral (..),
    PositionedExpression (..),
    PositionData (..),
    ParseExpression (..),
    Assignment (..),
    Statement (..),
  )
where

import Control.Monad.Combinators.Expr
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types (Dimension (..), Operation (..), PedantParseError (..), PrimitiveDim (..), Type (..))

-- | We now define a parser for this typed language
data Assignment = Assignment
  { assignmentName :: String,
    assignmentArguments :: [String],
    assignmentExpression :: PositionedExpression
  }
  deriving (Show)

data Statement
  = AssignmentStatement Assignment
  | UnitStatement [String]

data PositionData = PositionData Int Int
  deriving (Show, Eq, Ord)

data Positioned a = Positioned PositionData a
  deriving (Show)

type PositionedExpression = Positioned ParseExpression

data ParseDimensionPart = ParseDimensionPart
  { pdpName :: String,
    pdpPower :: Int
  }
  deriving (Show)

data ParseDimension
  = PowParseDim [Positioned ParseDimensionPart]
  | NormalParseDim [Positioned ParseDimensionPart]
  deriving (Show)

data ParseLiteral
  = ParseNumber Double ParseDimension
  | ParseList [PositionedExpression]
  | ParseRecord (Map.Map String PositionedExpression)
  deriving (Show)

data ParseExpression
  = PBinOp Operation PositionedExpression PositionedExpression
  | PVariable String
  | PConstant ParseLiteral
  | PNegate PositionedExpression
  | PAccess PositionedExpression String
  deriving (Show)

data Function = NaturalLogarithm
  deriving (Show)

type Parser = Parsec Void Text

sc :: Parser ()
sc =
  L.space
    hspace1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

foldLexeme :: Parser a -> Parser a
foldLexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pVariable :: Parser PositionedExpression
pVariable = position (PVariable <$> pName)

pTypedNumber :: Parser () -> Parser ParseLiteral
pTypedNumber sc' = do
  num <- L.lexeme sc (try L.float <|> L.decimal)
  ParseNumber num <$> parseDimension sc'

pTypedList :: Parser () -> Parser ParseLiteral
pTypedList sc' = do
  _ <- L.lexeme sc' (char '[')
  ParseList <$> startList
  where
    startList :: Parser [PositionedExpression]
    startList = do
      num <- L.lexeme sc' $ pExpr sc'
      rest <- ([] <$ L.lexeme sc' (char ']')) <|> (L.lexeme sc' (char ',') >> startList)
      return (num : rest)

pTypedRecord :: Parser () -> Parser ParseLiteral
pTypedRecord sc' = do
  _ <- L.lexeme sc' (char '{')
  ParseRecord . Map.fromList <$> startRecord
  where
    startRecord :: Parser [(String, PositionedExpression)]
    startRecord = do
      name <- L.lexeme sc' pName
      _ <- L.lexeme sc' $ char '='
      value <- L.lexeme sc' $ pExpr sc'
      rest <- ([] <$ L.lexeme sc' (char '}')) <|> (L.lexeme sc' (char ',') >> startRecord)
      return ((name, value) : rest)

parseDimension :: Parser () -> Parser ParseDimension
parseDimension sc' = (PowParseDim <$> try (char '^' *> parseNextDim [])) <|> (NormalParseDim <$> pLoop [])
  where
    pLoop :: [Positioned ParseDimensionPart] -> Parser [Positioned ParseDimensionPart]
    pLoop p = parseNextDim p <|> return p

    parseNextDim :: [Positioned ParseDimensionPart] -> Parser [Positioned ParseDimensionPart]
    parseNextDim oldDim = do
      dim <- L.lexeme sc' (position pSingleDim)
      pLoop (dim : oldDim)

    pSingleDim :: Parser ParseDimensionPart
    pSingleDim = do
      name <- (:) <$> letterChar <*> many letterChar
      number <- (fromInteger <$> L.signed (pure ()) L.decimal) <|> return 1
      return (ParseDimensionPart name number)

pConstant :: Parser () -> Parser PositionedExpression
pConstant sc' = position (PConstant <$> (pTypedList sc' <|> pTypedRecord sc' <|> pTypedNumber sc'))

pName :: Parser String
pName = (:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "name"

pTerm :: Parser () -> Parser PositionedExpression
pTerm sc' =
  let mainExpression =
        choice
          [ parens (pExpr sc'),
            L.lexeme sc' pVariable,
            pConstant sc'
          ]
   in do
        expression <- mainExpression
        L.lexeme sc' (accessor expression <|> return expression)
  where
    accessor :: PositionedExpression -> Parser PositionedExpression
    accessor expr = do
      _ <- char '.'
      L.lexeme sc' $ position (PAccess expr <$> pName)

pLine :: Parser (Maybe Statement)
pLine =
  choice
    [ Just <$> pStatement,
      return Nothing
    ]

program :: Parser [Statement]
program = catMaybes <$> many (sc *> pLine <* newline) <* eof

pStatement :: Parser Statement
pStatement = L.lineFold scn $ \sc' ->
  let new_space_consumer = try sc' <|> sc
   in pUnit new_space_consumer <|> (AssignmentStatement <$> pAssignment new_space_consumer)

pUnit :: Parser () -> Parser Statement
pUnit sc' = do
  L.lexeme sc (symbol "unit")
  UnitStatement <$> (pName `sepBy1` sc)

pAssignment :: Parser () -> Parser Assignment
pAssignment sc' = do
  name <- lexeme pName
  arguments <- many (lexeme pName)
  _ <- symbol "="
  sc'
  Assignment name arguments <$> pExpr sc'

scn :: Parser ()
scn =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

pExpr :: Parser () -> Parser PositionedExpression
pExpr sc' =
  makeExprParser (pTerm sc') (operatorTable sc')

operatorTable :: Parser () -> [[Operator Parser PositionedExpression]]
operatorTable sc' =
  [ [binary sc' "" (PBinOp App)],
    [binary sc' "^" (PBinOp Power)],
    [ prefix sc' "-" PNegate
    ],
    [ binary sc' "*" (PBinOp Mult),
      binary sc' "/" (PBinOp Div)
    ],
    [ binary sc' "+" (PBinOp Add),
      binary sc' "-" (PBinOp Sub)
    ]
  ]

position :: Parser a -> Parser (Positioned a)
position parser = do
  offset <- getOffset
  x <- parser
  newOffset <- getOffset
  return (Positioned (PositionData offset (newOffset - offset)) x)

binary :: Parser () -> Text -> (PositionedExpression -> PositionedExpression -> ParseExpression) -> Operator Parser PositionedExpression
binary sc' name f =
  InfixL
    ( do
        L.symbol sc' name
        return (combinePositions f)
    )

combinePositions :: (Positioned a -> Positioned b -> c) -> Positioned a -> Positioned b -> Positioned c
combinePositions f a@(Positioned startPos@(PositionData startOffset _) _) b@(Positioned (PositionData start2 length) _) =
  Positioned (PositionData startOffset ((start2 + length) - startOffset)) (f a b)

prefix :: Parser () -> Text -> (PositionedExpression -> ParseExpression) -> Operator Parser PositionedExpression
prefix sc' name f =
  Prefix
    ( do
        offset <- getOffset
        L.symbol sc' name
        return (\x@(Positioned (PositionData childOffset length) _) -> Positioned (PositionData offset ((childOffset + length) - offset)) (f x))
    )

errorBundleToPedantError :: ShowErrorComponent b => ParseErrorBundle Text b -> NonEmpty PedantParseError
errorBundleToPedantError bundle =
  let (SourcePos _ column row) = pstateSourcePos (bundlePosState bundle)
   in fmap
        ( \err ->
            PedantParseError
              (parseErrorTextPretty err)
              (unPos column - 1)
              (unPos row - 1)
              (unPos row - 1)
              (unPos column)
              (errorBundlePretty bundle)
        )
        (bundleErrors bundle)

parseProgram :: String -> T.Text -> Either (NonEmpty PedantParseError) [Statement]
parseProgram name contents = do
  Bifunctor.first errorBundleToPedantError $ parse program name contents
