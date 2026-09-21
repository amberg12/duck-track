{-# LANGUAGE StrictData #-}

module Generator.Board
    ( Color (..)
    , Piece (..)
    , Board (..)
    , parseFen
    )
where

import Control.Applicative
import Control.Monad (guard, (>=>))
import Data.Char (isDigit, isLower, isUpper)
import Data.Map.Strict qualified as Map
import Generator.Geometry qualified as G
import Text.Read (readMaybe)

data Color
    = White
    | Black
    deriving (Show, Eq)

data Piece
    = Duck
    | Pawn Color
    | Knight Color
    | Bishop Color
    | Rook Color
    | King Color
    | Queen Color
    deriving (Show, Eq)

type BoardPieces = Map.Map G.Square Piece

type Castling = ([G.File], [G.File])

data Board = Board
    { boardPieces :: BoardPieces
    , boardColor :: Color
    , boardCastling :: Castling
    , boardEnPassant :: Maybe G.Square
    , boardHalfMove :: Int
    , boardFullMove :: Int
    }
    deriving (Show)

parseFen :: String -> Maybe Board
parseFen fen = case words fen of
    [pieces, color, castling, enPassant, halfMove, fullMove] -> do
        parsedPieces <- parsePieces pieces
        parsedColor <- parseColor color
        parsedCastling <- parseCastling castling
        parsedEnPassant <- parseEnPassant enPassant
        parsedHalfMove <- parseHalfMove halfMove
        parsedFullMove <- parseFullMove fullMove

        pure $
            Board
                { boardPieces = parsedPieces
                , boardColor = parsedColor
                , boardCastling = parsedCastling
                , boardEnPassant = parsedEnPassant
                , boardHalfMove = parsedHalfMove
                , boardFullMove = parsedFullMove
                }
    _ -> Nothing

parsePiece :: Char -> Maybe Piece
parsePiece 'P' = Just (Pawn White)
parsePiece 'N' = Just (Knight White)
parsePiece 'B' = Just (Bishop White)
parsePiece 'R' = Just (Rook White)
parsePiece 'Q' = Just (Queen White)
parsePiece 'K' = Just (King White)
parsePiece 'p' = Just (Pawn Black)
parsePiece 'n' = Just (Knight Black)
parsePiece 'b' = Just (Bishop Black)
parsePiece 'r' = Just (Rook Black)
parsePiece 'q' = Just (Queen Black)
parsePiece 'k' = Just (King Black)
parsePiece '*' = Just Duck
parsePiece _ = Nothing

parsePieces :: String -> Maybe BoardPieces
parsePieces pieces = case p of
    Nothing -> Nothing
    Just (m, _) -> Just m
  where
    p = foldl f (Just (Map.empty, G.a8)) pieces
    f :: Maybe (BoardPieces, G.Square) -> Char -> Maybe (BoardPieces, G.Square)
    f Nothing _ = Nothing
    f (Just (m, prevSq)) c
        | c == '/' = do
            incRank <- prevSq `G.offsetSquare` (0, -1)
            Just (m, G.Square{file = G.FileA, rank = G.rank incRank})
        | isDigit c = do
            nextSquare <- prevSq `G.offsetSquare` (read [c], 0) <|> Just prevSq
            Just (m, nextSquare)
        | otherwise = do
            piece <- parsePiece c
            nextSquare <- prevSq `G.offsetSquare` (1, 0) <|> Just prevSq
            let newMap = Map.insert prevSq piece m
            Just (newMap, nextSquare)

parseColor :: String -> Maybe Color
parseColor "w" = Just White
parseColor "b" = Just Black
parseColor _ = Nothing

parseCastling :: String -> Maybe Castling
parseCastling = foldl f (Just ([], []))
  where
    f Nothing _ = Nothing
    f _ '-' = Just ([], [])
    f (Just (wc, bc)) 'K' = Just (wc ++ [G.FileH], bc)
    f (Just (wc, bc)) 'Q' = Just (wc ++ [G.FileA], bc)
    f (Just (wc, bc)) 'k' = Just (wc, bc ++ [G.FileH])
    f (Just (wc, bc)) 'q' = Just (wc, bc ++ [G.FileA])
    f (Just (wc, bc)) c =
        case G.fileFromChar c of
            Nothing -> Nothing
            Just file
                | isUpper c -> Just (wc ++ [file], bc)
                | isLower c -> Just (wc, bc ++ [file])
                | otherwise -> Nothing

parseEnPassant :: String -> Maybe (Maybe G.Square)
parseEnPassant "-" = Just Nothing
parseEnPassant sq = do
    sq' <- G.parseSquare sq
    pure $ Just sq'

parseHalfMove :: String -> Maybe Int
parseHalfMove = readMaybe >=> (\x -> x <$ guard (x >= 0))

parseFullMove :: String -> Maybe Int
parseFullMove = readMaybe >=> (\x -> x <$ guard (x >= 1))
