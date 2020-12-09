module Reducer (reduce, eval) where

import Types(Expression(..), Oper(..), IvyException(..), Progn(..), Option(..))
import Control.Exception (Exception, throw, try)

-- perform variable substitution with debrunjin indices
subst :: Expression -> Integer -> Expression -> Expression
subst e1 s e2 =
  case e2 of
    LSymbol s1   -> if s == s1 then e1 else LSymbol s1
    Fn e3        -> Fn (subst e1 (s + 1) e3)
    RecFn e3     -> RecFn (subst e1 (s + 2) e3)
    App e3 e4    -> App (subst e1 s e3) (subst e1 s e4)
    If e3 e4 e5  -> If (subst e1 s e3) (subst e1 s e4) (subst e1 s e5)
    Op o e3 e4   -> Op o (subst e1 s e3) (subst e1 s e4)
    _ -> e2
  
  
reduce :: Expression -> IO Expression
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

-- reduce of a value is just a value
reduce e = case e of
  IInteger _ -> return e
  IBoolean _ -> return e
  LSymbol _  -> return e
  GSymbol _  -> throw (Stuck e)
  RecFn _    -> return e
  Fn _       -> return e
  

-- reduce for programs
global_subst :: (String -> Option Expression) -> Expression -> Expression
global_subst g expr =
  case expr of
    GSymbol s -> case g s of
      Some x -> x
      None -> expr
    Fn e         -> Fn (global_subst g e)
    RecFn e      -> RecFn (global_subst g e)
    App e1 e2    -> App (global_subst g e1) (global_subst g e2)
    If e1 e2 e3  -> If (global_subst g e1) (global_subst g e2) (global_subst g e3)
    Op o e1 e2   -> Op o (global_subst g e1) (global_subst g e2)
    _ -> expr
  
  
eval :: Progn -> (String -> Option Expression) -> IO (Expression, String -> Option Expression)
eval prog g = case prog of
  Expr e -> do
    result <- try (reduce e)
    case result of
      --- this is not a correct implementation of the substitution semantics...
      Left (Stuck v) -> eval (Expr (global_subst g e)) g
      Right val -> return (val, g)
  Def x e -> 
    return (IBoolean False, (\s -> if s == x then Some e else g s))
  Seq p1 p2 -> do
    res <- eval p1 g
    eval p2 (snd res)


  



