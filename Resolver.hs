{-# LANGUAGE MultiWayIf #-}
module Resolver(scope_progn) where

import Control.Exception (Exception, throw)
import Types(Option(..), Expression(..), RawExpression(..), Progn(..), RawProgn(..), IvyException(..))

  
scope_expr :: RawExpression -> (String -> Option Integer) -> Expression
scope_expr raw indices =
  case raw of
    Symbol_Raw s -> case indices s of
      None   -> GSymbol s
      Some n -> LSymbol n

    Fn_Raw sym r ->
      Fn (scope_expr r (\x -> if x == sym
                             then Some 0
                             else case indices x of
                               Some n -> Some (n + 1)
                               None -> None))

    RecFn_Raw sym1 sym2 r -> do
      RecFn (scope_expr r (\x -> if
                               | x == sym1 -> Some 1 -- the function name is at index 1
                               | x == sym2 -> Some 0 -- the formal parameter is at index 0
                               | otherwise ->
                                 case indices x of
                                   Some n -> Some (n + 2)
                                   None -> None))

    App_Raw r1 r2  ->
      (App (scope_expr r1 indices) (scope_expr r2 indices))

    Op_Raw o r1 r2 -> 
      (Op o (scope_expr r1 indices) (scope_expr r2 indices))

    If_Raw r1 r2 r3 ->
      (If (scope_expr r1 indices) (scope_expr r2 indices) (scope_expr r3 indices))

    IInteger_Raw n  -> (IInteger n)
    IBoolean_Raw b  -> (IBoolean b)
  


scope_progn :: RawProgn -> Progn
scope_progn rawprog = 
  case rawprog of
    RawExpr e ->
      Expr (scope_expr e (\x -> None))
    RawDef x e ->
      Def x (scope_expr e (\x -> None))
    RawSeq p1 p2 ->
      Seq (scope_progn p1) (scope_progn p2)
