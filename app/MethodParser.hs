{-# LANGUAGE InstanceSigs #-}
module MethodParser where

import           Control.Applicative (Alternative (..))
import           Data.Char           (isDigit, isLetter, isSpace)
import           Data.List           (nub)

data Expr
  = Bin String Expr Expr
  | Var String
  | Lit Int
  deriving (Eq, Ord, Show)

data Error i e
  = EndOfInput
  | Unexpected i
  | CustomError e
  | Empty
  deriving (Eq, Show)

newtype Parser i e a
  = Parser { runParser :: [i] -> Either [Error i e] (a, [i]) }

instance Functor (Parser i e) where
  fmap :: (a -> b) -> Parser i e a -> Parser i e b
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    Right (f output, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (p1', rest) <- p1 input
    (output, rest') <- p2 rest
    Right (p1' output, rest')

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err'            -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

instance Monad (Parser i e) where
  return = pure

  Parser p >>= f = Parser $ \input -> do
    (output, rest) <- p input
    runParser (f output) rest

char :: (Eq i) => i -> Parser i e i
char x = Parser p
  where
    p (y:ys)
      | y == x = Right (x, ys)
      | otherwise = Left [Unexpected y]
    p [] = Left [Empty]

spanP :: (Char -> Bool) -> Parser Char e String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
      in Right (token, rest)

notNull :: Parser a e [a] -> Parser a e [a]
notNull (Parser p) =
  Parser $ \input -> do
    (xs, rest) <- p input
    if null xs
      then Left [Empty]
      else Right (xs, rest)

ws :: Parser Char e String
ws = spanP isSpace

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

exprVar :: Parser Char e Expr
exprVar = Var <$> (ws *> notNull vs <* ws)
  where vs = spanP isLetter

exprLit :: Parser Char e Expr
exprLit = f <$> (ws *> notNull (spanP isDigit) <* ws)
  where f ds = Lit $ read ds

exprParen :: (Eq e) => Parser Char e Expr
exprParen = ws *> char '(' *> expr <* char ')' <* ws

exprAddSub :: (Eq e) => Parser Char e Expr
exprAddSub = do
  left <- exprTerm
  operator <- string "+" <|> string "-"
  Bin operator left <$> expr

exprTerm :: (Eq e) => Parser Char e Expr
exprTerm = exprMulDiv <|> exprFactor

exprMulDiv :: (Eq e) => Parser Char e Expr
exprMulDiv = do
  left <- exprFactor
  operator <- string "*" <|> string "/"
  Bin operator left <$> exprTerm

exprFactor :: (Eq e) => Parser Char e Expr
exprFactor = exprParen <|> exprLit <|> exprVar

expr :: (Eq e) => Parser Char e Expr
expr = exprAddSub <|> exprTerm
