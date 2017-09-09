module Eval
( eval
, isTheorem
, testEval

-- TODO DELETE
, getVars
) where

import Control.Applicative ((<$>), liftA2)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Parser (ParseError, testParser)
import Types

newtype EvalError = EvalError String deriving (Eq, Show)

-- Map of AST UnaryOp type to boolean operator
unaryOps :: M.Map UnaryOp (Bool -> Bool)
unaryOps = M.fromList [ (Neg, not) ]

-- Map of AST BinaryOp type to boolean operator
binaryOps :: M.Map BinaryOp (Bool -> Bool -> Bool)
binaryOps = M.fromList [ (Eq, (==))
                       , (Or, (||))
                       , (And, (&&))
                       , (Implies, \p q -> not (p && not q))
                       ]

-- Returns the evaluation of an expression, if all values are defined
eval :: M.Map String Bool -> Expression -> Either EvalError Bool
eval _ (Val val) = Right val
eval m (Var var) = case M.lookup var m of
  Just val -> Right val
  Nothing -> Left . EvalError $ "No value found for " ++ var
eval m (UnaryExp op e) = unaryOps M.! op <$> eval m e
eval m (BinaryExp op e1 e2) = do
  v1 <- eval m e1
  v2 <- eval m e2
  Right $ (binaryOps M.! op) v1 v2

-- Get variable names of an expression
getVars :: Expression -> S.Set String
getVars (Val val) = S.empty
getVars (Var var) = S.singleton var
getVars (UnaryExp _ e) = getVars e
getVars (BinaryExp _ e1 e2) = S.union (getVars e1) (getVars e2)

-- Determines if the expression is a theorem
isTheorem :: Expression -> Bool
isTheorem expr = all verify maps
  where
    maps = M.fromList <$> foldr (liftA2 (:)) [[]] pairs
    pairs = [ [(v, True), (v, False)] | v <- vars ]
    vars = S.toList $ getVars expr
    verify m = either (const False) id $ eval m expr

-- Utility

testEval :: M.Map String Bool -> String -> Either ParseError (Either EvalError Bool)
testEval m str = case testParser str of
  Left parseError -> Left parseError
  Right exp -> case eval m exp of
    Left evalError -> Right $ Left evalError
    Right val -> Right $ Right val