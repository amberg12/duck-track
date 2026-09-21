module Generator.Run (run) where

import Generator.OptParse (Options (..), Positions (..))

run :: Options -> String
run opts = case positions opts of
    DFen dFen -> dFen ++ "\n"
