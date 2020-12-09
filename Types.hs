module Types (
  IvyException(..),
  Option(..),
  Oper(..),
  Expression(..),
  Progn(..),
  RawExpression(..),
  RawProgn(..),
  AST(..)) where

import Control.Exception (Exception, throw)

data Option a = None | Some a
  deriving Show

data Oper = Plus | Minus | GTEQ | Mul
  deriving Show

data Expression = IInteger Integer
  | IBoolean Bool
  | LSymbol Integer
  | GSymbol String
  | Op Oper Expression Expression
  | If Expression Expression Expression
  | App Expression Expression
  | Fn Expression
  | RecFn Expression
  deriving Show

data Progn = Expr Expression | Seq Progn Progn | Def String Expression
  deriving Show

data RawExpression = IInteger_Raw Integer
  | IBoolean_Raw Bool
  | Symbol_Raw String
  | App_Raw RawExpression RawExpression
  | Op_Raw Oper RawExpression RawExpression
  | If_Raw RawExpression RawExpression RawExpression
  | Fn_Raw String RawExpression
  | RecFn_Raw String String RawExpression
  deriving Show

data RawProgn = RawExpr RawExpression | RawSeq RawProgn RawProgn | RawDef String RawExpression
  deriving Show

data AST = Atom String | List [AST]
  deriving Show

data IvyException = ParseError AST | IllFormedError RawExpression | Stuck Expression
  deriving Show
instance Exception IvyException
