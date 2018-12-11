{-# LANGUAGE NamedFieldPuns #-}
module Lib.Day3 (
    run
) where

import Text.Regex.TDFA ((=~))
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Lib.Matrix (Matrix, foldMatrix, updateWithDefault, empty)

data BoundingBox = BoundingBox {
    bid, x, y, w, h :: Int
} deriving (Show)

readBB :: String -> BoundingBox
readBB line =
    let bid:x:y:w:h:_ = map read $ tail $ head $ line =~ "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
    in BoundingBox {bid, x, y, w, h}

fill :: Matrix [Int] -> BoundingBox -> Matrix [Int]
fill m BoundingBox{bid, x, y, w, h} =
    let idxs = [(x, y) | x <- [x..x+w-1], y <- [y..y+h-1]]
    in foldl (\acc idx -> updateWithDefault ((:) bid) idx [] acc) m idxs

mapMultiCells :: (a -> [Int] -> a) -> a -> Matrix [Int] -> a
mapMultiCells f = foldMatrix (\acc ids -> if length ids > 1 then f acc ids else acc)

getClaims :: IO [BoundingBox]
getClaims = map readBB . lines <$> readFile "input\\day03"

run = do
    claims <- getClaims
    let filled = foldl fill empty claims
    print $ mapMultiCells (\acc _ -> acc + 1) 0 filled
    let ids = S.fromList $ map bid claims
    print $ head $ S.elems $ mapMultiCells (foldl (flip S.delete)) ids filled
