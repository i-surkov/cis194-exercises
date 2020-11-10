{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Week05.Calc
  ( eval,
    evalStr,
    Expr (..),
    MinMax (..),
    Mod7 (..),
    compile,
    HasVars (..),
    VarExprT (..),
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Week05.ExprT
import Week05.Parser
import qualified Week05.StackVM as SVM

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

--------------------------------------------------- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

--------------------------------------------------- Exercise 3

-- This is our type class :)
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- now write an instance for our ExprT type
instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

--------------------------------------------------- Exercise 4
-- Write instances for Integer, Bool, MinMax, and Mod7

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax i1) (MinMax i2) = lit $ max i1 i2
  mul (MinMax i1) (MinMax i2) = lit $ min i1 i2

instance Expr Mod7 where
  lit i = Mod7 $ i `mod` 7
  add (Mod7 i1) (Mod7 i2) = lit $ i1 + i2
  mul (Mod7 i1) (Mod7 i2) = lit $ i1 * i2

--------------------------------------------------- Exercise 5

instance Expr SVM.Program where
  lit x = [SVM.PushI x]
  add p1 p2 = p1 ++ p2 ++ [SVM.Add]
  mul p1 p2 = p1 ++ p2 ++ [SVM.Mul]

compile :: String -> SVM.Program
compile = fromMaybe [] . parseExp lit add mul

--------------------------------------------------- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT
  = LitWithVar Integer
  | AddWithVar VarExprT VarExprT
  | MulWithVar VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = LitWithVar
  add = AddWithVar
  mul = MulWithVar

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add one two myMap = one myMap >>= (\x -> (x +) <$> two myMap)
  mul one two myMap = one myMap >>= (\x -> (x *) <$> two myMap)
