module Types (
  IvyException(..),
  Option(..),
  Oper(..),
  Expr(..),
  RawExpr(..),
  AST(..)) where

import Control.Exception (Exception, throw)

data Option a = None | Some a
  deriving Show

data Oper = Plus | Minus | GTEQ | Mul
  deriving Show

data Expr = IInteger Integer
          | IBoolean Bool
          | Symbol Integer
          | Op Oper Expr Expr
          | If Expr Expr Expr
          | App Expr Expr
          | Fn Expr
          | RecFn Expr
  deriving Show

data RawExpr = IInteger_Raw Integer
          | IBoolean_Raw Bool
          | Symbol_Raw String
          | App_Raw RawExpr RawExpr
          | Op_Raw Oper RawExpr RawExpr
          | If_Raw RawExpr RawExpr RawExpr
          | Fn_Raw String RawExpr
          | RecFn_Raw String String RawExpr
  deriving Show

data AST = Atom String | List [AST]
  deriving Show

data IvyException = ParseError AST | IllFormedError RawExpr | Stuck Expr
  deriving Show
instance Exception IvyException
