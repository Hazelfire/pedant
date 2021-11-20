{-# LANGUAGE OverloadedStrings #-}

-- | The parser for pedant. Creates a syntax tree from the file
module Parser
  ( parseProgram,
    PedantParseError (..),
    Operation (..),
    TypedExpression (..),
    PositionedExpression (..),
    PositionData (..),
    ParseExpression (..),
    Assignment (..),
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
import Types (Dimension (..), Operation (..), PedantParseError (..))

-- | We now define a parser for this typed language
data Assignment = Assignment String PositionedExpression
  deriving (Show)

newtype PositionData = PositionData Int
  deriving (Show, Eq, Ord)

data PositionedExpression = PositionedExpression PositionData ParseExpression
  deriving (Show)

data TypedExpression
  = TypedNumber Double Dimension
  | TypedList [PositionedExpression]
  deriving (Show)

data ParseExpression
  = PBinOp Operation PositionedExpression PositionedExpression
  | PVariable String
  | PConstant TypedExpression
  | PNegate PositionedExpression
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

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pVariable :: Parser PositionedExpression
pVariable = PositionedExpression <$> getPositionData <*> (PVariable <$> lexeme pName)

getPositionData :: Parser PositionData
getPositionData = PositionData <$> getOffset

pTypedNumber :: Parser TypedExpression
pTypedNumber = do
  num <- lexeme (try L.float <|> L.decimal)
  TypedNumber num <$> parseDimension

pTypedList :: Parser TypedExpression
pTypedList = do
  _ <- lexeme (char '[')
  TypedList <$> startList
  where
    startList :: Parser [PositionedExpression]
    startList = do
      num <- pExpr
      rest <- ([] <$ lexeme (char ']')) <|> (lexeme (char ',') >> startList)
      return (num : rest)

parseDimension :: Parser Dimension
parseDimension = (PowDim <$> try (char '^' *> parseNextDim Map.empty)) <|> (NormDim <$> pLoop Map.empty)
  where
    pLoop :: Map.Map String Int -> Parser (Map.Map String Int)
    pLoop p = parseNextDim p <|> return p

    parseNextDim :: Map.Map String Int -> Parser (Map.Map String Int)
    parseNextDim oldDim = do
      (name, power) <- lexeme pSingleDim
      pLoop (Map.insert name power oldDim)

    pSingleDim :: Parser (String, Int)
    pSingleDim = do
      name <- (:) <$> letterChar <*> many letterChar
      number <- (fromInteger <$> L.signed (pure ()) L.decimal) <|> return 1
      return (name, number)

pConstant :: Parser PositionedExpression
pConstant = PositionedExpression <$> getPositionData <*> (PConstant <$> (pTypedList <|> pTypedNumber))

pName :: Parser String
pName = (:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "variable"

pTerm :: Parser PositionedExpression
pTerm =
  choice
    [ parens pExpr,
      pVariable,
      pConstant
    ]

pLine :: Parser (Maybe Assignment)
pLine =
  choice
    [ Just <$> pAssignment,
      return Nothing
    ]

program :: Parser [Assignment]
program = catMaybes <$> many (sc *> pLine <* newline) <* eof

pAssignment :: Parser Assignment
pAssignment = do
  name <- lexeme pName
  _ <- symbol "="
  Assignment name <$> pExpr

pExpr :: Parser PositionedExpression
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser PositionedExpression]]
operatorTable =
  [ [binary "" (PBinOp App)],
    [binary "^" (PBinOp Power)],
    [ prefix "-" PNegate,
      prefix "+" (\(PositionedExpression _ a) -> a)
    ],
    [ binary "*" (PBinOp Mult),
      binary "/" (PBinOp Div)
    ],
    [ binary "+" (PBinOp Add),
      binary "-" (PBinOp Sub)
    ]
  ]

binary :: Text -> (PositionedExpression -> PositionedExpression -> ParseExpression) -> Operator Parser PositionedExpression
binary name f =
  InfixL
    ( do
        symbol name
        pos <- getPositionData
        return (\a b -> PositionedExpression pos (f a b))
    )

prefix, postfix :: Text -> (PositionedExpression -> ParseExpression) -> Operator Parser PositionedExpression
prefix name f =
  Prefix
    ( do
        symbol name
        pos <- getPositionData
        return (PositionedExpression pos . f)
    )
postfix name f =
  Postfix
    ( do
        symbol name
        pos <- getPositionData
        return (PositionedExpression pos . f)
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
              (errorBundlePretty bundle)
        )
        (bundleErrors bundle)

parseProgram :: String -> T.Text -> Either (NonEmpty PedantParseError) [Assignment]
parseProgram name contents = do
  Bifunctor.first errorBundleToPedantError $ parse program name contents
