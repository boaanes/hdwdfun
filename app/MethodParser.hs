{-# LANGUAGE InstanceSigs #-}
module MethodParser where

import           Control.Applicative (Alternative (..))
import           Data.Char           (isDigit, isSpace)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Var String
  | Lit Int
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    Just (f output, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)

  Parser p1 <*> Parser p2 = Parser $ \input -> do
    (p1', rest) <- p1 input
    (output, rest') <- p2 rest
    Just (p1' output, rest')

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing

  Parser l <|> Parser r =
    Parser $ \input -> l input <|> r input

instance Monad Parser where
  return = pure

  Parser p >>= f = Parser $ \input -> do
    (output, rest) <- p input
    runParser (f output) rest

alphabet :: String
alphabet = take 26 ['a','b'..] ++ take 26 ['A', 'B']

char :: Char -> Parser Char
char x = Parser p
  where
    p (y:ys)
      | y == x = Just (x, ys)
      | otherwise = Nothing
    p [] = Nothing

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
      in Just (token, rest)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (xs, rest) <- p input
    if null xs
      then Nothing
      else Just (xs, rest)

ws :: Parser String
ws = spanP isSpace

string :: String -> Parser String
string = traverse char

exprVar :: Parser Expr
exprVar = Var <$> (ws *> notNull vs <* ws)
  where vs = spanP (`elem` alphabet)

exprLit :: Parser Expr
exprLit = f <$> (ws *> notNull (spanP isDigit) <* ws)
  where f ds = Lit $ read ds

exprAdd :: Parser Expr
exprAdd = do
  a <- exprLit <|> exprVar
  _ <- char '+'
  b <- exprLit <|> exprVar
  return $ Add a b

exprSub :: Parser Expr
exprSub = do
  a <- exprLit <|> exprVar
  _ <- char '-'
  b <- exprLit <|> exprVar
  return $ Sub a b

exprMul :: Parser Expr
exprMul = do
  a <- exprLit <|> exprVar
  _ <- char '*'
  b <- exprLit <|> exprVar
  return $ Mul a b

exprDiv :: Parser Expr
exprDiv = do
  a <- exprLit <|> exprVar
  _ <- char '/'
  b <- exprLit <|> exprVar
  return $ Div a b

exprParen :: Parser Expr
exprParen = char '(' *> expr <* char ')'

expr :: Parser Expr
expr = exprAdd <|> exprSub <|> exprMul <|> exprDiv <|> exprLit <|> exprVar <|> exprParen
