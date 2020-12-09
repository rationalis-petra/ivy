module Parser (parse) where

import Types (Oper(..),  AST(..), RawExpression(..), IvyException(..), RawProgn(..))
import Control.Exception (Exception, throw)

-- parse, /but/ returns either an expression or exception
parse :: AST -> IO RawProgn
parse ast = case ast of
  Atom _      -> throw (ParseError ast)
  List (x:[]) -> dodef x
  List (x:xs) -> do
    r1 <- dodef x
    r2 <- parse (List xs)
    return (RawSeq r1 r2)
  where
    -- perform definition substition if applicable
    dodef p = case p of
      List [(Atom "def"), (Atom x), s] -> do
        r <- eparse s
        case r of
          Fn_Raw y e -> return (RawDef x (RecFn_Raw x y e ))
          _ -> return (RawDef x r)
      _ -> do
        r <- eparse p
        return (RawExpr r)

eparse :: AST -> IO RawExpression
eparse ast = 
  case ast of 
    Atom str
      | numberp str -> return (numberparse str)
      | boolp str   -> return (boolparse str)
      | otherwise   -> return (Symbol_Raw str)
    List xs -> case xs of 
      [(Atom "+"), s1, s2] -> do
        r1 <- eparse s1
        r2 <- eparse s2
        return (Op_Raw Plus r1 r2)

      [(Atom "-"), s1, s2] -> do
        r1 <- eparse s1
        r2 <- eparse s2
        return (Op_Raw Minus r1 r2)

      [(Atom "*"), s1, s2] -> do
        r1 <- eparse s1
        r2 <- eparse s2
        return (Op_Raw Mul r1 r2)

      [(Atom ">="), s1, s2] -> do
        r1 <- eparse s1
        r2 <- eparse s2
        return (Op_Raw GTEQ r1 r2)

      [(Atom "fn"), (Atom x), (Atom y), s] -> do
        r <- eparse s
        return (RecFn_Raw x y r)

      [(Atom "fn"), (Atom x), s] -> do
        r <- eparse s
        return (Fn_Raw x r)

      [(Atom "if"), s1, s2, s3] -> do
        r1 <- eparse s1
        r2 <- eparse s2
        r3 <- eparse s3
        return (If_Raw r1 r2 r3)

      [s1, s2] -> do
        r1 <- eparse s1
        r2 <- eparse s2
        return (App_Raw r1 r2)

      _ -> throw (ParseError ast)
  where
    numberp string =
      (any ((head string) ==) "-1234567890") && (all (\x -> any (x ==) "1234567890") (tail string))
    numberparse string = IInteger_Raw (read string)
    boolp string = string == "nil" || string == "t"
    boolparse string = IBoolean_Raw (string == "t")
