module Generator.Run (run) where

import Data.Maybe (fromMaybe)
import Generator.Board (Board (..), parseFen)
import Generator.OptParse (Options (..), Positions (..))

run :: Options -> IO String
run opts = case positions opts of
    DFen dFen -> pure $ fromMaybe "bad fen" (show <$> parseFen dFen)
