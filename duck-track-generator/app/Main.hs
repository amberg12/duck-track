module Main
    ( main
    ) where

import Generator.OptParse qualified as OP
import Generator.Run qualified as R

main :: IO ()
main = OP.parse >>= putStr . R.run
