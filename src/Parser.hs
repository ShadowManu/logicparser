module Parser
( 
) where

import Control.Applicative ((<$>))
import Control.Monad.Identity (Identity)
import Data.List (find)

import Text.Parsec hiding (parse, State, Stream)
import Text.Parsec.Expr

import Types
import Lexer

-- Token Pickers

type Picker a = Token -> Maybe a

true TRUE = Just $ Val True
true _ = Nothing

false FALSE = Just $ Val False
false _ = Nothing

var :: Picker Expression
var (VAR n) = Just $ Var n
var _ = Nothing

negation NOT = Just Neg
negation _ = Nothing

equivalence EQUIV = Just Eq
equivalence _ = Nothing

disjunction OR = Just Or
disjunction _ = Nothing

conjunction AND = Just And
conjunction _ = Nothing

implication IMPLIES = Just Implies
implication _ = Nothing

lParen LPAREN = Just LPAREN
lParen _ = Nothing

rParen RPAREN = Just RPAREN
rParen _ = Nothing

-- Expression Parser

type State = ()
type Stream = [Token]
type Parser a = Parsec Stream State a

-- Given a Picker, create a standard token Parser
parse :: Picker a -> Parser a
parse = tokenPrim show (const . const)

-- Operations helpers
prefix picker = Prefix $ UnaryExp <$> parse picker
binary picker = Infix $ BinaryExp <$> parse picker

table :: [[Operator Stream State Identity Expression]]
table = [ [ prefix negation ]
        , [ binary conjunction AssocLeft ]
        , [ binary disjunction AssocLeft ]
        , [ binary implication AssocRight ]
        , [ binary equivalence AssocLeft ]
        ]

-- High level parsers

basic = parse true
        <|> parse false
        <|> parse var

term = basic
       <|> try (between (parse lParen) (parse rParen) basic)

expr = buildExpressionParser table term

-- Utility

testParser :: Parser a -> Stream -> Either ParseError a
testParser parser = runParser parser () "Fuente"