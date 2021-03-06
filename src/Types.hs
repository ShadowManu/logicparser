module Types where

---- Token Types

data Token =
  -- UNARY OPS
  NOT
  -- BINARY OPS
  | EQUIV |  OR | AND | IMPLIES
  -- LITERAL VALUES
  | TRUE | FALSE
  -- SYMBOLS
  | LPAREN | RPAREN
  -- VARIABLES
  | VAR String
  deriving (Show, Eq)

---- Expression Types

data UnaryOp = Neg deriving (Show, Eq)
             
data BinaryOp = Eq
              | Or
              | And
              | Implies
              deriving (Show, Eq)

data Expression = Var { getVar :: String }
                | Val { getVal :: Bool }
                | BinaryExp BinaryOp Expression Expression
                | UnaryExp UnaryOp Expression
                deriving (Show, Eq)