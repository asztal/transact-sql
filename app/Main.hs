module Main where

import qualified Language.TransactSql.Parser as P

main :: IO ()
main = do
  str <- readLn
  print (P.run P.parseQuery "stdin" str)
