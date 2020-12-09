module Main where
  
import Lexer (lexer)
import Parser (parse)
import Resolver (scope_progn)
import Reducer (eval, reduce)

import Types(Progn(..), Option(..), AST(..))

import Control.Monad 

main = 
  loop (\x -> None) where
  loop g = do
    inp <- getLine  
    if inp == "quit"
      then return ()
      else do
        raw    <- parse (lexer inp) 
        (v, g) <- eval (scope_progn raw) g
        print v
        loop g
   
