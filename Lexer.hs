module Lexer ( lexer ) where

import Types ( AST(..) )

balanced string count =
  if string == [] || count < 0 then (count == 0)
  else case string of
    '(':cs -> balanced cs (count + 1)
    ')':cs -> balanced cs (count - 1)
    c:cs   -> balanced cs count
         
reverse_ast :: AST -> AST
reverse_ast x = case x of
  Atom string -> Atom (reverse string)
  List xs     -> List (map reverse_ast (reverse xs))

lexer :: String -> AST
lexer string =(fst (rlexer string [] (List [])))


rlexer :: String -> String -> AST -> (AST, String)
rlexer string sym ast = 
  if string == [] then
    case ast of  
      List xs -> if sym /= [] then (reverse_ast (List ((Atom sym):xs)), [])
                 else (reverse_ast ast, [])
  else
    case string of
      '\n':cs -> case ast of
                  List xs ->
                    if sym /= [] then rlexer cs [] (List ((Atom sym):xs))
                    else rlexer cs [] (List xs)
      ' ':cs -> case ast of
                  List xs ->
                    if sym /= [] then rlexer cs [] (List ((Atom sym):xs))
                    else rlexer cs [] (List xs)
      '(':cs -> case ast of
                  List xs ->
                    let (sub_ast, rest) = rlexer cs [] (List [])
                    in if sym /= [] then rlexer rest [] (List (sub_ast:(Atom sym):xs))
                    else rlexer rest [] (List (sub_ast:xs))
      ')':cs -> case ast of
                   List xs ->
                     if sym /= [] then (List ((Atom sym):xs), cs) 
                     else (List xs, cs)
      c:cs   -> rlexer cs (c:sym) ast


