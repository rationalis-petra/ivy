module Main where
  
import Lexer (lexer)
import Parser (parse)
import Resolver (resolve_scope)
import Reducer (reduce)

import Types(Expr(..), Option(..), AST(..))

import Control.Monad 

main = do   
    inp <- getLine  
    if inp == "quit"
      then return ()
      else do
        raw    <- parse (lexer inp) 
        prog   <- resolve_scope raw (\x -> None)
        result <- reduce prog
        print result
        main
