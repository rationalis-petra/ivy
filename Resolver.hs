{-# LANGUAGE MultiWayIf #-}
module Resolver(resolve_scope) where

import Control.Exception (Exception, throw)
import Types(Option(..), Expr(..), RawExpr(..), IvyException(..))

--resolve_scope :: RawExpr -> (String -> Option Integer) -> Exception Expr IvyException
resolve_scope raw indices =
  case raw of
    Symbol_Raw s -> case indices s of
      None   -> throw (IllFormedError raw)
      Some n -> return (Symbol n)

    Fn_Raw sym r -> do 
      e <- resolve_scope r (\x -> if x == sym
                             then Some 0
                             else case indices x of
                               Some n -> Some (n + 1)
                               None -> None);
           return (Fn e)

    RecFn_Raw sym1 sym2 r -> do
      e <- resolve_scope r (\x -> if
                               | x == sym1 -> Some 1 -- the function name is at index 1
                               | x == sym2 -> Some 0 -- the formal parameter is at index 0
                               | otherwise ->
                                 case indices x of
                                   Some n -> Some (n + 2)
                                   None -> None);
           return (RecFn e)

    App_Raw r1 r2  -> do
      e1 <- resolve_scope r1 indices
      e2 <- resolve_scope r2 indices
      return (App e1 e2)

    Op_Raw o r1 r2  -> do
      e1 <- resolve_scope r1 indices
      e2 <- resolve_scope r2 indices
      return (Op o e1 e2)

    If_Raw r1 r2 r3 -> do
      e1 <- resolve_scope r1 indices
      e2 <- resolve_scope r2 indices
      e3 <- resolve_scope r3 indices
      return (If e1 e2 e3)

    IInteger_Raw n  -> return (IInteger n)
    IBoolean_Raw b  -> return (IBoolean b)
  

