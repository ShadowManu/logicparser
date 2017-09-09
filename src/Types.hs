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
  deriving (Eq, Show)

---- Expression Types

data UnaryOp = Neg deriving (Eq, Ord, Show)
             
data BinaryOp = Eq
              | Or
              | And
              | Implies
              deriving (Eq, Ord, Show)

data Expression = Val { getVal :: Bool }
                | Var { getVar :: String }
                | UnaryExp UnaryOp Expression
                | BinaryExp BinaryOp Expression Expression
                deriving (Eq, Show)