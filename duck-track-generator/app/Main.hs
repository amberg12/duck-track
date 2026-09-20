module Main
  ( main
  ) where

import qualified Generator.Add as G

main :: IO ()
main = do
  print $ G.add 2 2
  