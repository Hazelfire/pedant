{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | The parser for pedant. Creates a syntax tree from the file
module Pedant.Parser
  ( parseProgram,
    PedantParseError (..),
    Positioned (..),
    Operation (..),
    ParseDimensionPart (..),
    ParseDimension (..),
    ParseLiteral (..),
    PositionData (..),
    ParseExpression (..),
    Assignment (..),
    Statement (..),
    makeErrorBundle,
  )
where

import Control.Monad.Combinators.Expr
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Pedant.InBuilt as InBuilt
import Pedant.Types (Operation (..), PedantParseError (..), PrettyPrint (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | c We now define a parser for this typed language
data Assignment = Assignment
  { assignmentName :: T.Text,
    assignmentArguments :: [T.Text],
    assignmentExpression :: PositionedExpression
  }
  deriving (Show)

data Statement
  = AssignmentStatement Assignment
  | UnitStatement [Positioned T.Text]
  | ImportStatement (Positioned T.Text) [Positioned T.Text]
  deriving (Show)

data PositionData = PositionData Int Int
  deriving (Show, Eq, Ord)

data Positioned a = Positioned PositionData a
  deriving (Show)

instance Eq a => Eq (Positioned a) where
  (==) (Positioned a1 b1) (Positioned a2 b2) = a1 == a2 && b1 == b2

instance Functor Positioned where
  fmap f (Positioned p x) = Positioned p (f x)

type PositionedExpression = Positioned ParseExpression

data ParseDimensionPart = ParseDimensionPart
  { pdpName :: T.Text,
    pdpPower :: Int
  }
  deriving (Show, Eq)

instance PrettyPrint ParseDimensionPart where
  pPrint (ParseDimensionPart name power) =
    if power == 1
      then name
      else T.concat [name, T.pack (show power)]

data ParseDimension
  = PowParseDim [Positioned ParseDimensionPart]
  | NormalParseDim [Positioned ParseDimensionPart]
  deriving (Show, Eq)

instance PrettyPrint ParseDimension where
  pPrint (NormalParseDim parts) = T.unwords (map pPrint parts)
  pPrint (PowParseDim parts) = "^" <> T.unwords (map pPrint parts)

data ParseLiteral
  = ParseNumber Double ParseDimension
  | ParseEmptyList
  | ParseRecord (Map.Map T.Text PositionedExpression)
  deriving (Show, Eq)

instance PrettyPrint ParseLiteral where
  pPrint (ParseNumber num dim) = T.unwords [T.pack $ show num, pPrint dim]
  pPrint ParseEmptyList = "[]"
  pPrint (ParseRecord op) =
    T.concat
      [ "{",
        T.intercalate ", " (map (\(key, value) -> T.concat [key, " = ", pPrint value]) (Map.toAscList op)),
        "}"
      ]

data ParseExpression
  = PBinOp T.Text PositionedExpression PositionedExpression
  | PVariable T.Text
  | PConstant ParseLiteral
  | PPrefix T.Text PositionedExpression
  | PAccess PositionedExpression T.Text
  deriving (Show, Eq)

instance PrettyPrint a => PrettyPrint (Positioned a) where
  pPrint (Positioned _ a) = pPrint a

instance PrettyPrint ParseExpression where
  pPrint (PBinOp op e1 e2) = T.unwords [pPrint e1, op, pPrint e2]
  pPrint (PVariable var) = var
  pPrint (PConstant lit) = pPrint lit
  pPrint (PPrefix op e1) = T.concat [op, pPrint e1]
  pPrint (PAccess e1 att) = T.concat [pPrint e1, att]

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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pVariable :: Parser PositionedExpression
pVariable = position (PVariable <$> pName)

pTypedNumber :: Parser () -> Parser ParseLiteral
pTypedNumber sc' = do
  num <- L.lexeme sc (try L.float <|> L.decimal)
  ParseNumber num <$> parseDimension sc'

-- Lists are parsed as an empty list with all elements concatenated into them
pTypedList :: Parser () -> Parser ParseExpression
pTypedList sc' = do
  _ <- L.lexeme sc' (char '[')
  choice
    [ PConstant ParseEmptyList <$ L.lexeme sc' (char ']'),
      startList
    ]
  where
    startList :: Parser ParseExpression
    startList = do
      num <- L.lexeme sc' $ pExpr sc'
      choice
        [ do
            _ <- L.lexeme sc' (char ',')
            PBinOp ":" num <$> position startList,
          PConstant ParseEmptyList <$ L.lexeme sc' (char ']')
        ]

pTypedRecord :: Parser () -> Parser ParseLiteral
pTypedRecord sc' = do
  _ <- L.lexeme sc' (char '{')
  ParseRecord . Map.fromList <$> startRecord
  where
    startRecord :: Parser [(T.Text, PositionedExpression)]
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
      number <- fromInteger <$> L.signed (pure ()) L.decimal <|> return 1
      return (ParseDimensionPart (T.pack name) number)

pConstant :: Parser () -> Parser PositionedExpression
pConstant sc' = position (pTypedList sc' <|> PConstant <$> (pTypedRecord sc' <|> pTypedNumber sc'))

pName :: Parser T.Text
pName = T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "name")

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
   in choice
        [ pImport new_space_consumer,
          pUnit new_space_consumer,
          AssignmentStatement <$> pAssignment new_space_consumer
        ]

-- | Import statements. They come in the form of:
-- import [module]([name1, name2, ...])
-- >>> :set -XOverloadedStrings
-- >>> parse (pImport sc) "" "import moduleName(name1, name2)\n"
-- Right (ImportStatement (Positioned (PositionData 7 10) "moduleName") [Positioned (PositionData 18 5) "name1",Positioned (PositionData 25 5) "name2"])
--
-- Empty imports are invalid:
-- >>> parse (pImport sc) "" "import moduleName()\n"
-- Left (ParseErrorBundle {bundleErrors = TrivialError 18 (Just (Tokens (')' :| ""))) (fromList [Label ('n' :| "ame")]) :| [], bundlePosState = PosState {pstateInput = "import moduleName()\n", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})
pImport :: Parser () -> Parser Statement
pImport sc' = do
  _ <- try (L.lexeme sc' (symbol "import"))
  fileName <- position $ L.lexeme sc' pName
  _ <- L.lexeme sc' (symbol "(")
  ImportStatement fileName <$> pImportList
  where
    pImportList :: Parser [Positioned T.Text]
    pImportList = do
      importName <- position $ L.lexeme sc' pName
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
  UnitStatement <$> (position pName `sepBy1` sc')

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
  let prefixOps = map (\op -> (InBuilt.opPrecedence op, prefix sc' (InBuilt.opName op) (PPrefix (InBuilt.opName op)))) InBuilt.inBuiltPrefixOperations
      binaryOps = map (\op -> (InBuilt.opPrecedence op, binary sc' (InBuilt.opName op) (PBinOp (InBuilt.opName op)))) InBuilt.inBuiltBinaryOperations
      combinedOps = prefixOps ++ binaryOps
      sortedOps = List.groupBy (\a b -> fst a == fst b) (List.sortOn fst combinedOps)
   in map (map snd) sortedOps

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
        _ <- L.symbol sc' name
        return (combinePositions f)
    )

combinePositions :: (Positioned a -> Positioned b -> c) -> Positioned a -> Positioned b -> Positioned c
combinePositions f a@(Positioned (PositionData startOffset _) _) b@(Positioned (PositionData start2 l) _) =
  Positioned (PositionData startOffset ((start2 + l) - startOffset)) (f a b)

prefix :: Parser () -> Text -> (PositionedExpression -> ParseExpression) -> Operator Parser PositionedExpression
prefix sc' name f =
  Prefix
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
  Bifunctor.first errorBundleToPedantError $ parse program name contents

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
