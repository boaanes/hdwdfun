{-# LANGUAGE InstanceSigs #-}
module MethodParser where

import           Control.Applicative (Alternative (..))
import           Data.Char           (isDigit, isLetter, isSpace)
import           Data.List           (nub)

data Expr
  = Bin String Expr Expr
  | Var String
  | Lit Int
  deriving (Show, Eq, Ord)

data Error i
  = EndOfInput
  | Unexpected i
  | Empty
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: String -> Either [Error Char] (a, String)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    Right (f output, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)

  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (p1', rest) <- p1 input
    (output, rest') <- p2 rest
    Right (p1' output, rest')

instance Alternative Parser where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err'            -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

instance Monad Parser where
  return = pure

  Parser p >>= f = Parser $ \input -> do
    (output, rest) <- p input
    runParser (f output) rest

char :: Char -> Parser Char
char x = Parser p
  where
    p (y:ys)
      | y == x = Right (x, ys)
      | otherwise = Left [Unexpected y]
    p [] = Left [Empty]

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
      in Right (token, rest)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (xs, rest) <- p input
    if null xs
      then Left [Empty]
      else Right (xs, rest)

ws :: Parser String
ws = spanP isSpace

string :: String -> Parser String
string = traverse char

exprVar :: Parser Expr
exprVar = Var <$> (ws *> notNull vs <* ws)
  where vs = spanP isLetter

exprLit :: Parser Expr
exprLit = f <$> (ws *> notNull (spanP isDigit) <* ws)
  where f ds = Lit $ read ds

exprParen :: Parser Expr
exprParen = ws *> char '(' *> expr <* char ')' <* ws

exprAddSub :: Parser Expr
exprAddSub = do
  left <- exprTerm
  operator <- string "+" <|> string "-"
  Bin operator left <$> expr

exprTerm :: Parser Expr
exprTerm = exprMulDiv <|> exprFactor

exprMulDiv :: Parser Expr
exprMulDiv = do
  left <- exprFactor
  operator <- string "*" <|> string "/"
  Bin operator left <$> exprTerm

exprFactor :: Parser Expr
exprFactor = exprParen <|> exprLit <|> exprVar

expr :: Parser Expr
expr = exprAddSub <|> exprTerm
