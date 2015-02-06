module Calc where

import           ExprT
import           Parser

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s =
  do e <- parseExp Lit Add Mul s
     return $ eval e
