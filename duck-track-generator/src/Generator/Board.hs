{-# LANGUAGE StrictData #-}

module Generator.Board
    ( Color (..)
    , Piece (..)
    , Board (..)
    , parseFen
    ) where

import Data.Char (isDigit)
import Control.Applicative

import Data.Map.Strict qualified as Map

import Generator.Geometry qualified as G

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
                Just (m, G.Square { file = G.FileA, rank = G.rank incRank})
            | isDigit c = do
                nextSquare <- prevSq `G.offsetSquare` (read [c], 0) <|> Just prevSq
                Just (m, nextSquare)
            | otherwise = do
                piece <- parsePiece c
                nextSquare <- prevSq `G.offsetSquare` (1, 0) <|> Just prevSq
                let newMap = Map.insert prevSq piece m
                Just (newMap, nextSquare)

parseColor :: String -> Maybe Color
parseColor = undefined

parseCastling :: String -> Maybe Castling
parseCastling = undefined

parseEnPassant :: String -> Maybe (Maybe G.Square)
parseEnPassant = undefined

parseHalfMove :: String -> Maybe Int
parseHalfMove = undefined

parseFullMove :: String -> Maybe Int
parseFullMove = undefined
