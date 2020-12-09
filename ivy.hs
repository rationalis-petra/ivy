module Main where
  
import Lexer (lexer)
import Parser (parse)
import Resolver (scope_progn)
import Reducer (eval, reduce)

import Types(Progn(..), Option(..), AST(..))

import Control.Exception(IOException, catch)
import System.Environment
import Control.Monad 

loop g = do
  inp <- getLine  
  if inp == "quit"
    then return ()
    else do
      raw    <- parse (lexer inp) 
      (v, g) <- eval (scope_progn raw) g
      print v
      loop g

run_file f = do
  file <- catch (readFile f)
                (\e -> do let err = show (e :: IOException)
                          print ("Warning: Couldn't open " ++ f ++ ": " ++ err)
                          return "");
  raw <- parse (lexer file)
  (v, g) <- eval (scope_progn raw) (\x -> None)
  print v

main = do args <- getArgs
          case args of
               [] -> loop (\x -> None)
               f:[] -> run_file f
               otherwise -> putStrLn "Program takes only 0 or 1 argument"

   
