{-# LANGUAGE OverloadedStrings #-}
-- | The parser for dimensional, a small dimensional programming language
module Test where
import qualified Data.Map.Ordered as OMap
import Data.Map.Ordered ((|<))
import qualified Data.Map as Map
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Maybe
import qualified Data.List as List
import Control.Monad.Combinators.Expr
import qualified System.Environment as Env
import qualified Data.Set as Set
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))


-- | Defining a shallow embedding for a typed number.
--   A typed number is a number with units. It must follow
--   the rules of dimensional analysis
data TypedNumber = TypedNumber Double Dimension
                 | InvalidTypedNumber String



instance Fractional TypedNumber where
  fromRational x = TypedNumber (fromRational x) dimNone
  recip (TypedNumber x dim) = either InvalidTypedNumber (TypedNumber (recip x)) (dimRecip dim)
  recip x = x

instance Floating TypedNumber where
    pi = dimensionless pi
    exp = const $ InvalidTypedNumber "Cannot find exponential"
    log a@(TypedNumber x dim) =
      if isDimensionless a then
        TypedNumber (log x) dim
      else
        InvalidTypedNumber "Cannot find log of dimensionful quantity"
    log x = x
    sqrt = const $ InvalidTypedNumber "Cannot find sqrt"
    (TypedNumber a (PowDim powDim)) ** (TypedNumber b typeDim) = either (const $ InvalidTypedNumber "Cannot multiply powers") (TypedNumber (a ** b)) (dimMult (NormDim powDim) typeDim)
    (TypedNumber x dim) ** other = InvalidTypedNumber $ "Left dimension is " ++ show dim ++ " but needs to be a power dimension to take to power " ++ show other
    _ ** _ = InvalidTypedNumber "Cannot find power with error"
    logBase _ _ = InvalidTypedNumber "Cannot use logBase"
    sin _ = InvalidTypedNumber "Cannot find sin"
    cos _ = InvalidTypedNumber "Cannot find cos"
    tan _ = InvalidTypedNumber "Cannot find tan"
    asin _ = InvalidTypedNumber "Cannot find asin"
    acos _ = InvalidTypedNumber "Cannot find acos"
    atan _ = InvalidTypedNumber "Cannot find atan"
    sinh _ = InvalidTypedNumber "Cannot find sinh"
    cosh _ = InvalidTypedNumber "Cannot find cosh"
    tanh _ = InvalidTypedNumber "Cannot find tanh"
    asinh _ = InvalidTypedNumber "Cannot find asinh"
    acosh _ = InvalidTypedNumber "Cannot find acosh"
    atanh _ = InvalidTypedNumber "Cannot find atanh"

instance Num TypedNumber where
  (TypedNumber x a) * (TypedNumber y b) = either InvalidTypedNumber (TypedNumber (x * y)) (dimMult a b)
  a * _ = a

  (TypedNumber x a) + (TypedNumber y b) = 
    if a == b then
      TypedNumber (x + y) a
    else
      InvalidTypedNumber $ "Could not add " ++ show a ++ " and " ++ show b
  a + _ = a

  negate (TypedNumber x a) = 
      TypedNumber (-x) a
  negate x = x
  signum (TypedNumber x a) =
    if x > 0 then
      TypedNumber 1 a
    else
      TypedNumber (-1) a
  signum x = x

  abs (TypedNumber x a) =
    if x > 0 then
      TypedNumber x a
    else
      TypedNumber (negate x) a
  abs a = a

  fromInteger x = TypedNumber (fromInteger x) dimNone

data Dimension = NormDim (Map.Map String Int)
                  | PowDim (Map.Map String Int)
  deriving (Eq)

instance Show Dimension where
  show (NormDim dim) = 
    if Map.size dim == 0 then
      "dimensionless"
    else
      unwords $ map (\(name, amount) -> if amount == 1 then name else name ++ show amount)  (List.sortOn (negate . snd) (Map.toList dim))
  show (PowDim dim) = 
    if Map.size dim == 1 then
      "^" ++ show (NormDim dim)
    else
      "^(" ++ show (NormDim dim) ++ ")"

dimRecip :: Dimension -> Either String Dimension
dimRecip (NormDim x) = Right $ NormDim (Map.map negate x)
dimRecip x = Left $ "Cannot find recip of " ++ show x

dimMult :: Dimension -> Dimension -> Either String Dimension
dimMult (NormDim a) (NormDim b) = Right . NormDim $ Map.filter (/=0) $ Map.unionWith (+) a b
dimMult x y = Left $ "Cannot multiply " ++ show x ++ " to " ++ show y

dimNone :: Dimension
dimNone = NormDim Map.empty

isDimensionless :: TypedNumber -> Bool
isDimensionless (TypedNumber _ dim) = dim == dimNone
isDimensionless _ = False


instance Show TypedNumber where
  show (TypedNumber val dim) = show val ++ " " ++ show dim
  show (InvalidTypedNumber err) = "Error: " ++ err


unit :: String -> Double -> TypedNumber
unit name value = TypedNumber value (NormDim $ Map.singleton name 1)

dimensionless :: Double -> TypedNumber
dimensionless value = TypedNumber value dimNone

-- | Some examples of dimensions
meters :: Double -> TypedNumber
meters = unit "m"

seconds :: Double -> TypedNumber
seconds = unit "s"



-- | We now define a parser for this typed language
data Assignment = Assignment String PositionedExpression
  deriving (Show)

newtype PositionData = PositionData Int
  deriving (Show, Eq, Ord)

data PositionedExpression = PositionedExpression PositionData Expression
  deriving (Show)

data Expression = EBinOp Operation PositionedExpression PositionedExpression
                | EVariable String
                | EConstant TypedNumber
                | ENegate PositionedExpression
  deriving (Show)

data Function = NaturalLogarithm
  deriving (Show)
          
data Operation = Add | Sub | Mult | Div | App | Power
  deriving (Show)


type Parser = Parsec Void Text
-- | Next we use Megaparsec (NOT PARSEC) to make our parser

sc :: Parser ()
sc = L.space
  hspace1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

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
pVariable = PositionedExpression <$> getPositionData <*> (EVariable <$> lexeme pName)

getPositionData :: Parser PositionData
getPositionData = PositionData <$> getOffset

pTypedNumber :: Parser TypedNumber
pTypedNumber = do
  num <- lexeme (try L.float <|> L.decimal)
  TypedNumber num <$> parseDimension

parseDimension :: Parser Dimension
parseDimension = (PowDim <$> (char '^' *> pLoop Map.empty)) <|> (NormDim <$> pLoop Map.empty)
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
     



pInteger :: Parser PositionedExpression
pInteger = PositionedExpression <$> getPositionData <*> (EConstant <$> pTypedNumber)

pName :: Parser String
pName = (:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "variable"

pTerm :: Parser PositionedExpression
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger
  ]

pLine :: Parser (Maybe Assignment)
pLine = choice
  [ Just <$> pAssignment
  , return Nothing
  ]

program :: Parser [Assignment]
program =  catMaybes <$> many (pLine <* newline) <* eof

pAssignment :: Parser Assignment
pAssignment = do
  name <- lexeme pName
  _ <- symbol "="
  Assignment name <$> pExpr

pExpr :: Parser PositionedExpression
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser PositionedExpression]]
operatorTable =
  [ [ binary "" (EBinOp App) ]
  , [ binary "^" (EBinOp Power) ]
  , [ prefix "-" ENegate
    , prefix "+" (\(PositionedExpression _ a) -> a)
    ]
  , [ binary "*" (EBinOp Mult)
    , binary "/" (EBinOp Div)
    ]
  , [ binary "+" (EBinOp Add)
    , binary "-" (EBinOp Sub)
    ]
  ]

binary :: Text -> (PositionedExpression -> PositionedExpression -> Expression) -> Operator Parser PositionedExpression
binary  name f = InfixL (do
  symbol name
  pos <- getPositionData
  return (\a b -> PositionedExpression pos (f a b))
  )

prefix, postfix :: Text -> (PositionedExpression -> Expression) -> Operator Parser PositionedExpression
prefix  name f = Prefix  (do
  symbol name
  pos <- getPositionData
  return (PositionedExpression pos . f)
  )
postfix  name f = Postfix  (do
  symbol name
  pos <- getPositionData
  return (PositionedExpression pos . f)
  )

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [] -> putStrLn "dimensional [file]"
    (name :_) -> do
      contents <- readFile name
      case parse program name (T.pack contents) of
        Right a ->
          case typeCheck Map.empty a of
            Right valid -> 
              forM_ (List.reverse (OMap.assocs $ executeProgram OMap.empty a)) $ \(name, value) ->
                putStrLn $ name  ++ " = "++ show value
            Left err@(TypeError (PositionData offset) _) -> do
              let initialPosState = PosState contents 0 (initialPos name) (mkPos 4) ""
                  (_, posState)= attachSourcePos id [offset] initialPosState
                  error = ParseErrorBundle (FancyError 0 (Set.singleton (ErrorCustom err)) :| [] ) posState
              putStrLn (errorBundlePretty error)
        Left b -> fail (errorBundlePretty b)

data TypeError = TypeError PositionData String
  deriving (Eq, Ord)

instance ShowErrorComponent TypeError where
  showErrorComponent (TypeError _ err) = err

typeCheck :: Map.Map String Dimension -> [Assignment] -> Either TypeError Bool
typeCheck values ((Assignment name expr): rest) =
  case typeCheckExpression values expr of
    Left str -> Left str
    Right value -> typeCheck (Map.insert name value values) rest 
typeCheck values [] = Right True

typeCheckExpression :: Map.Map String Dimension -> PositionedExpression -> Either TypeError Dimension
typeCheckExpression values (PositionedExpression pos expression) =
  let toTypeError = Bifunctor.first (TypeError pos)
  in
  case expression of
    EBinOp Mult x y -> do
      x1 <- typeCheckExpression values x 
      x2 <- typeCheckExpression values y
      toTypeError $ dimMult x1 x2
    EBinOp Div x y -> do
      x1 <- typeCheckExpression values x 
      x2 <- typeCheckExpression values y >>= (toTypeError <$> dimRecip)
      toTypeError $ dimMult x1 x2
    EBinOp Add x y -> do
      xType <- typeCheckExpression values x
      yType <- typeCheckExpression values y
      if xType == yType then
        return xType
      else
        Left . TypeError pos $ "Could not add " ++ show xType ++ " and " ++ show yType
    EBinOp Sub x y -> do
      xType <- typeCheckExpression values x
      yType <- typeCheckExpression values y
      if xType == yType then
        return xType
      else
        Left .  TypeError pos $ "Could not subtract " ++ show xType ++ " and " ++ show yType
    EBinOp Power x y -> do
      xType <- typeCheckExpression values x
      yType <- typeCheckExpression values y
      case xType of
        PowDim childDim -> 
            toTypeError $ dimMult (NormDim childDim) yType
        NormDim _->
            Left $ TypeError pos $ "Could not power " ++ show xType ++ " to " ++ show yType ++ " base needs to be power type"
    EBinOp App (PositionedExpression _ (EVariable "ln")) x -> do
      xType <- typeCheckExpression values x
      case xType of
        PowDim childDim ->
          Right $ NormDim childDim
        NormDim dim ->
         if dim == Map.empty then
          Right $ NormDim dim
         else
           Left $ TypeError pos $ "Count not take log of " ++ show xType ++ " must be power type"
    EBinOp App (PositionedExpression _ (EVariable name)) x -> 
      maybe (Left . TypeError pos $ "No Such variable " ++ name) Right (Map.lookup name values)
    EBinOp App (PositionedExpression _ (EConstant val)) x -> Left . TypeError pos $ "Cannot evaluate constant"
    EBinOp App (PositionedExpression _ (ENegate val)) x -> Left . TypeError pos $ "Cannot apply negation"
    EBinOp App _ x -> Left . TypeError pos $ "Could not evalute function"

    EVariable name ->  
      case Map.lookup name values of
        Just value -> Right value
        Nothing -> Left . TypeError pos $ "Could not find variable " ++ name
    EConstant (TypedNumber _ dim) -> return dim
    EConstant (InvalidTypedNumber err) -> Left . TypeError pos $ "INVALID STATE, TYPE ERROR IN PARSE TREE"
    ENegate expr -> typeCheckExpression values expr
  

executeProgram :: OMap.OMap String TypedNumber -> [Assignment] -> OMap.OMap String TypedNumber
executeProgram values ((Assignment name expr) : rest) =
  let value = evaluateExpression values expr
  in 
    executeProgram ((name, value) |< values) rest 
executeProgram values [] = values
  
evaluateExpression :: OMap.OMap String TypedNumber -> PositionedExpression -> TypedNumber
evaluateExpression values (PositionedExpression _ expression) =
  case expression of
    EBinOp Mult x y -> evaluateExpression values x * evaluateExpression values y
    EBinOp Div x y -> evaluateExpression values x / evaluateExpression values y
    EBinOp Add x y -> evaluateExpression values x + evaluateExpression values y
    EBinOp Sub x y -> evaluateExpression values x - evaluateExpression values y
    EBinOp Power x y -> evaluateExpression values x ** evaluateExpression values y
    EBinOp App (PositionedExpression _ (EVariable "ln")) x -> log $ evaluateExpression values x

    EBinOp App (PositionedExpression _ (EVariable name)) x -> InvalidTypedNumber $ "No such function "  ++ name
    EBinOp App (PositionedExpression _ (EConstant val)) x -> InvalidTypedNumber $ "Cannot call constant "  ++ show val
    EBinOp App (PositionedExpression _ (EBinOp op x1 x2)) x -> InvalidTypedNumber $ "Cannot call operator "  ++ show x1 ++ " " ++ show x2
    EBinOp App (PositionedExpression _ (ENegate val)) x -> InvalidTypedNumber $ "Cannot call negation of "  ++ show val

    EVariable name ->  
      case OMap.lookup name values of
        Just value -> value
        Nothing -> InvalidTypedNumber $  "Could not find variable " ++ name
    EConstant number -> number
    ENegate expr ->  negate (evaluateExpression values expr)

