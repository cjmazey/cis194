{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import qualified Data.Map as M
import           ExprT
import           Parser
import qualified StackVM  as S

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s =
  do e <- parseExp Lit Add Mul s
     return $ eval e

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr S.Program where
  lit n = [S.PushI n]
  add p1 p2 = p1 ++ p2 ++ [S.Add]
  mul p1 p2 = p1 ++ p2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = LitV Integer
              | AddV VarExprT VarExprT
              | MulV VarExprT VarExprT
              | VarV String
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = LitV
  add = AddV
  mul = MulV

instance HasVars VarExprT where
  var = VarV

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . return
  add e1 e2 m = do r1 <- e1 m
                   r2 <- e2 m
                   return $ r1 + r2
  mul e1 e2 m = do r1 <- e1 m
                   r2 <- e2 m
                   return $ r1 * r2

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
