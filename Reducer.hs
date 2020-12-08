module Reducer ( reduce ) where
  -- val = integers | nil | t
  -- op = + | - | >=
  -- e ::= val | e1 e2 | (e) | if e1 e2 e3 | op e1 e2 | fn x e

import Types(Expr(..), Oper(..), IvyException(..))
import Control.Exception (Exception, throw)

-- perform variable substitution with debrunjin indices
subst :: Expr -> Integer -> Expr -> Expr 
subst e1 s e2 =
  case e2 of
    Symbol s1    -> if s == s1 then e1 else Symbol s1
    Fn e3        -> Fn (subst e1 (s + 1) e3)
    RecFn e3     -> RecFn (subst e1 (s + 2) e3)
    App e3 e4    -> App (subst e1 s e3) (subst e1 s e4)
    If e3 e4 e5  -> If (subst e1 s e3) (subst e1 s e4) (subst e1 s e5)
    Op o e3 e4   -> Op o (subst e1 s e3) (subst e1 s e4)
    IInteger n   -> IInteger n
    IBoolean b   -> IBoolean b
  
  
--reduce :: Expr -> Exception Expr IvyException
reduce (Op op l r) = do
  v1 <- reduce l
  v2 <- reduce r
  case (v1, v2) of
    (IInteger n1, IInteger n2) -> case op of
      Plus  -> return (IInteger (n1 + n2))
      Minus -> return (IInteger (n1 - n2))
      Mul   -> return (IInteger (n1 * n2))
      GTEQ  -> return (IBoolean (n1 >= n2))
    _ -> throw (Stuck (Op op l r))

reduce (If e1 e2 e3) = do
  cond <- reduce e1
  case cond of
    IBoolean False -> reduce e3 
    _ -> reduce e2

reduce (App e1 e2) = do
  fn <- reduce e1
  case fn of
    Fn e -> reduce (subst e2 0 e)
    RecFn e -> reduce (App (Fn (subst (RecFn e) 1 e)) e2)
    _ -> throw (Stuck (App e1 e2))

-- rreduce of a value/exception is just a value/exception
reduce e = case e of
  IInteger _ -> return (e)
  IBoolean _ -> return (e)
  Symbol _   -> return (e)
  RecFn _    -> return (e)
  Fn _       -> return (e)
  
