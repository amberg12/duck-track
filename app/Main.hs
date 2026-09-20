module Main (main) where

import qualified DuckTrack.Add as D

main :: IO ()
main = do
  print $ D.add 2 2
