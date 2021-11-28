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
import Types (Dimension (..), Operation (..), PedantParseError (..))

-- | We now define a parser for this typed language
data Assignment = Assignment String PositionedExpression
  deriving (Show)

data Statement
  = AssignmentStatement Assignment
  | UnitStatement [String]

newtype PositionData = PositionData Int
  deriving (Show, Eq, Ord)

data PositionedExpression = PositionedExpression PositionData ParseExpression
  deriving (Show)

data TypedExpression
  = TypedNumber Double Dimension
  | TypedList [PositionedExpression]
  | TypedRecord (Map.Map String PositionedExpression)
  deriving (Show)

data ParseExpression
  = PBinOp Operation PositionedExpression PositionedExpression
  | PVariable String
  | PConstant TypedExpression
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
pVariable = PositionedExpression <$> getPositionData <*> (PVariable <$> pName)

getPositionData :: Parser PositionData
getPositionData = PositionData <$> getOffset

pTypedNumber :: Parser () -> Parser TypedExpression
pTypedNumber sc' = do
  num <- L.lexeme sc (try L.float <|> L.decimal)
  TypedNumber num <$> parseDimension sc'

pTypedList :: Parser () -> Parser TypedExpression
pTypedList sc' = do
  _ <- L.lexeme sc' (char '[')
  TypedList <$> startList
  where
    startList :: Parser [PositionedExpression]
    startList = do
      num <- L.lexeme sc' $ pExpr sc'
      rest <- ([] <$ L.lexeme sc' (char ']')) <|> (L.lexeme sc' (char ',') >> startList)
      return (num : rest)

pTypedRecord :: Parser () -> Parser TypedExpression
pTypedRecord sc' = do
  _ <- L.lexeme sc' (char '{')
  TypedRecord . Map.fromList <$> startRecord
  where
    startRecord :: Parser [(String, PositionedExpression)]
    startRecord = do
      name <- L.lexeme sc' pName
      _ <- L.lexeme sc' $ char '='
      value <- L.lexeme sc' $ pExpr sc'
      rest <- ([] <$ L.lexeme sc' (char '}')) <|> (L.lexeme sc' (char ',') >> startRecord)
      return ((name, value) : rest)

parseDimension :: Parser () -> Parser Dimension
parseDimension sc' = (PowDim <$> try (char '^' *> parseNextDim Map.empty)) <|> (NormDim <$> pLoop Map.empty)
  where
    pLoop :: Map.Map String Int -> Parser (Map.Map String Int)
    pLoop p = parseNextDim p <|> return p

    parseNextDim :: Map.Map String Int -> Parser (Map.Map String Int)
    parseNextDim oldDim = do
      (name, power) <- L.lexeme sc' pSingleDim
      pLoop (Map.insert name power oldDim)

    pSingleDim :: Parser (String, Int)
    pSingleDim = do
      name <- (:) <$> letterChar <*> many letterChar
      number <- (fromInteger <$> L.signed (pure ()) L.decimal) <|> return 1
      return (name, number)

pConstant :: Parser () -> Parser PositionedExpression
pConstant sc' = PositionedExpression <$> getPositionData <*> (PConstant <$> (pTypedList sc' <|> pTypedRecord sc' <|> pTypedNumber sc'))

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
        accessor expression <|> return expression
  where
    accessor :: PositionedExpression -> Parser PositionedExpression
    accessor expr = do
      _ <- char '.'
      PositionedExpression <$> getPositionData <*> (PAccess expr <$> L.lexeme sc' pName)

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
  _ <- symbol "="
  sc'
  Assignment name <$> pExpr sc'

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
    [ prefix sc' "-" PNegate,
      prefix sc' "+" (\(PositionedExpression _ a) -> a)
    ],
    [ binary sc' "*" (PBinOp Mult),
      binary sc' "/" (PBinOp Div)
    ],
    [ binary sc' "+" (PBinOp Add),
      binary sc' "-" (PBinOp Sub)
    ]
  ]

binary :: Parser () -> Text -> (PositionedExpression -> PositionedExpression -> ParseExpression) -> Operator Parser PositionedExpression
binary sc' name f =
  InfixL
    ( do
        L.symbol sc' name
        pos <- getPositionData
        return (\a b -> PositionedExpression pos (f a b))
    )

prefix, postfix :: Parser () -> Text -> (PositionedExpression -> ParseExpression) -> Operator Parser PositionedExpression
prefix sc' name f =
  Prefix
    ( do
        L.symbol sc' name
        pos <- getPositionData
        return (PositionedExpression pos . f)
    )
postfix sc' name f =
  Postfix
    ( do
        L.symbol sc' name
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

parseProgram :: String -> T.Text -> Either (NonEmpty PedantParseError) [Statement]
parseProgram name contents = do
  Bifunctor.first errorBundleToPedantError $ parse program name contents
