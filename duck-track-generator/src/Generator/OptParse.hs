module Generator.OptParse
    ( Options (..)
    , Positions (..)
    , parse
    )
where

import Options.Applicative

data Positions
    = DFen String

data Options = Options
    { positions :: Positions
    }

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts =
    info (parseOptions <**> helper) description
  where
    description =
        fullDesc
            <> header "duck-track-generator"
            <> progDesc "Filter wakformat, epd and fens to generate mate-track style epds"

parseOptions :: Parser Options
parseOptions = Options <$> positionsParser

positionsParser :: Parser Positions
positionsParser =
    DFen <$> strOption argInfo
  where
    argInfo = long "dfen" <> short 'f' <> help "DuckFen to generate mate score from"
