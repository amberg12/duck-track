module Generator.Geometry
    ( Square (..)
    , File (..)
    , Rank (..)
    , offsetSquare
    , fileFromChar
    , rankFromChar
    , parseSquare
    , a1
    , a8
    )
where

import Data.Char (toUpper)

data Square = Square
    { file :: !File
    , rank :: !Rank
    }
    deriving (Show, Eq, Ord)

data File
    = FileA
    | FileB
    | FileC
    | FileD
    | FileE
    | FileF
    | FileG
    | FileH
    deriving (Show, Eq, Enum, Bounded, Ord)

data Rank
    = Rank1
    | Rank2
    | Rank3
    | Rank4
    | Rank5
    | Rank6
    | Rank7
    | Rank8
    deriving (Show, Eq, Enum, Bounded, Ord)

offsetSquare :: Square -> (Int, Int) -> Maybe Square
offsetSquare s (x, y)
    | not $ inBound newFile = Nothing
    | not $ inBound newRank = Nothing
    | otherwise = Just $ Square (toEnum newFile) (toEnum newRank)
  where
    newFile = (+) x $ fromEnum $ file s
    newRank = (+) y $ fromEnum $ rank s
    inBound n = 0 <= n && n <= 7

fileFromChar :: Char -> Maybe File
fileFromChar c
    | c' >= 'A' && c' <= 'H' = Just (toEnum (fromEnum c' - fromEnum 'A'))
    | otherwise = Nothing
  where
    c' = toUpper c

rankFromChar :: Char -> Maybe Rank
rankFromChar c
    | c >= '1' && c <= '8' = Just (toEnum (fromEnum c - fromEnum '1'))
    | otherwise = Nothing

parseSquare :: String -> Maybe Square
parseSquare [f, r] = do
    f' <- fileFromChar f
    r' <- rankFromChar r
    pure $ Square{file = f', rank = r'}
parseSquare _ = Nothing

a1 :: Square
a1 = Square{file = FileA, rank = Rank1}

a8 :: Square
a8 = Square{file = FileA, rank = Rank8}
