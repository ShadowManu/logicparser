module Parser where

import Text.Parsec

import Types
import Lexer

type Lista = [Int]

type LogicParser a = Parsec [Token] () a

-- Primitive parsers

prim :: (Token -> Maybe a) -> LogicParser a
prim = tokenPrim show (const . const)

true :: LogicParser Expression
true = prim consume
  where
   consume TRUE = Just $ Val True
   consume _ = Nothing

false :: LogicParser Expression
false = prim consume
  where
    consume FALSE = Just $ Val False
    consume _ = Nothing

var :: LogicParser Expression
var = prim consume
  where
    consume (VAR n) = Just $ Var n
    consume _ = Nothing

unaryOp :: LogicParser UnaryOp
unaryOp = prim consume
  where
    consume NOT = Just Neg
    consume _ = Nothing

binaryOp :: LogicParser BinaryOp
binaryOp = prim consume
  where
    consume EQUAL = Just Eq
    consume NOTEQUAL = Just NotEq
    consume OR = Just Or
    consume AND = Just And
    consume IMPLIES = Just Implies 
    consume _ = Nothing

lParen :: LogicParser Token
lParen  = prim consume
  where
    consume LPAREN = Just LPAREN
    consume _ = Nothing

rParen :: LogicParser Token
rParen = prim consume
  where
    consume RPAREN = Just RPAREN
    consume _ = Nothing

-- Higher level parsers

bool :: LogicParser Expression
bool = true <|> false

atom :: LogicParser Expression
atom = bool <|> var

expr :: LogicParser Expression
expr = try unaryExpr
       <|> try (between lParen rParen expr)
       <|> try binaryExpr
       <|> atom

unaryExpr :: LogicParser Expression
unaryExpr = do
  op <- unaryOp
  e <- expr
  return $ UnaryExp op e

binaryExpr :: LogicParser Expression
binaryExpr = do
  e1 <- atom
  op <- binaryOp
  e2 <- atom
  return $ BinaryExp e1 op e2

-- Utility

testParser :: LogicParser a -> [Token] -> Either ParseError a
testParser parser = runParser parser () "Fuente"