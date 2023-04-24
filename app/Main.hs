module Main where

import System.IO
import Exp
import Parsing
import Printing
import REPLCommand
import Lab2
import Eval
import Sugar

main :: IO ()
main
  = do
    putStr "miniHaskell> "
    hFlush stdout
    s <- getLine
    case parseFirst replCommand s of
          Nothing -> putStrLn "Cannot parse command" >> main
          Just Quit -> return ()
          Just (Load _) -> putStrLn "Not implemented" >> main
          Just (Eval es) ->
            case parseFirst exprParser es of
              Nothing -> putStrLn "Error: cannot parse expression" >> main 
              Just e ->
                let simpleE = desugarExp e
                    simpleE' = normalize simpleE
                    e' = sugarExp simpleE'
                 in putStrLn (showExp e') >> main