{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module AST
    ( Expr (..)
    , Parser
    , Value (..)
    , parseExpr
    , parseValue
    , parseVar
    ) where

import           Control.Monad                  (guard, void)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Byte.Lexer     (lexeme)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     (decimal, float, signed)

type Parser = Parsec Void String

data Value
  = DoubleVal Double
  | BoolVal Bool
  deriving (Eq, Ord)

instance Show Value where
  show (DoubleVal d) = show d
  show (BoolVal b)   = show b

data Expr
  = BinOp String Expr Expr
  | UnOp String Expr
  | Var String
  | Lit Value
  deriving (Eq, Ord, Show)

ws :: Parser ()
ws = void $ takeWhileP (Just "space") (== ' ')

symbol :: String -> Parser String
symbol s = ws *> string s <* ws

parseValue :: Parser Value
parseValue = try (DoubleVal <$> (parseInt <|> parseDouble)) <|> (BoolVal <$> parseBool)
  where
    parseDouble = signed ws $ lexeme ws float
    parseInt = signed ws $ lexeme ws (fromIntegral @Integer @Double <$> decimal)
    parseBool = lexeme ws ((True <$ string "true") <|> (False <$ string "false"))

reserved :: [String]
reserved = ["True", "False", "sqrt", "log", "!"]

parseVar :: Parser Expr
parseVar = do
  var <- lexeme ws (some letterChar)
  guard (var `notElem` reserved)
  return $ Var var

parseKeyword :: Parser Expr
parseKeyword = choice
  [ Lit (BoolVal True) <$ lexeme ws (string "True")
  , Lit (BoolVal False) <$ lexeme ws (string "False")
  , UnOp <$> symbol "!" <*> parseFactor
  , UnOp <$> symbol "sqrt" <* symbol "(" <*> parseExpr <* symbol ")"
  , UnOp <$> symbol "log" <* symbol "(" <*> parseExpr <* symbol ")"
  ]

parseFactor :: Parser Expr
parseFactor = parseParen <|> parseLit <|> try parseKeyword <|> parseVar
  where
    parseParen = between (symbol "(") (symbol ")") parseExpr
    parseLit = Lit <$> parseValue

parseExpr :: Parser Expr
parseExpr = makeExprParser parseFactor operatorTable
  where
    operatorTable =
        [ [ InfixL (BinOp <$> symbol "*")
          , InfixL (BinOp <$> symbol "/")
          ]
        , [ InfixL (BinOp <$> symbol "+")
          , InfixL (BinOp <$> symbol "-")
          ]
        , [ InfixL (BinOp <$> symbol "==")
          , InfixL (BinOp <$> symbol "!=")
          ]
        ]
