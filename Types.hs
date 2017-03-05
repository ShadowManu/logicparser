module Types where

---- Operators


---- Tokens

data Token =
  -- UNARY OPS
  NOT
  -- BINARY OPS
  | EQUAL | NOTEQUAL | OR | AND | IMPLIES
  -- LITERAL VALUES
  | TRUE | FALSE
  -- SYMBOLS
  | LPAREN | RPAREN
  -- VARIABLES
  | VAR String
  deriving (Show, Eq)

--- Other stuff

data UnaryOp = Neg deriving (Show, Eq)
             
data BinaryOp = Eq
              | NotEq
              | Or
              | And
              | Implies
              deriving (Show, Eq)

data Expression = Var { getName :: String }
                | Val { getVal :: Bool }
                | BinaryExp Expression BinaryOp Expression
                | UnaryExp UnaryOp Expression
                deriving (Show, Eq)