module Main
    ( main
    ) where

import Generator.Add qualified as G

main :: IO ()
main = do
    print $ G.add 2 2
