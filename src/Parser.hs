module Parser
( testParser
, true
) where

import Text.Parsec hiding (Stream, State)

import Types
import Lexer

type State = ()
type Stream = [Token]

type Parser a = Parsec Stream State a

-- Primitive parsers

-- Given a matcher function, create a standard Parser
prim :: (Token -> Maybe a) -> Parser a
prim = tokenPrim show (const . const)

true :: Parser Expression
true = prim match
  where
   match TRUE = Just $ Val True
   match _ = Nothing

false :: Parser Expression
false = prim match
  where
    match FALSE = Just $ Val False
    match _ = Nothing

var :: Parser Expression
var = prim match
  where
    match (VAR n) = Just $ Var n
    match _ = Nothing

unaryOp :: Parser UnaryOp
unaryOp = prim match
  where
    match NOT = Just Neg
    match _ = Nothing

binaryOp :: Parser BinaryOp
binaryOp = prim match
  where
    match EQUAL = Just Eq
    match NOTEQUAL = Just NotEq
    match OR = Just Or
    match AND = Just And
    match IMPLIES = Just Implies 
    match _ = Nothing

lParen :: Parser Token
lParen  = prim match
  where
    match LPAREN = Just LPAREN
    match _ = Nothing

rParen :: Parser Token
rParen = prim match
  where
    match RPAREN = Just RPAREN
    match _ = Nothing

-- Higher level parsers

bool :: Parser Expression
bool = true <|> false

-- term :: Parser Expression
-- term = try (between lParen rParen expr)
--        <|> var
--        <|> bool

-- Utility

testParser :: Parser a -> Stream -> Either ParseError a
testParser parser = runParser parser () "Fuente"