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

term :: LogicParser Expression
term = try (between lParen rParen expr)
       <|> var
       <|> bool

-- Utility

testParser :: LogicParser a -> [Token] -> Either ParseError a
testParser parser = runParser parser () "Fuente"