{-# LANGUAGE NamedFieldPuns #-}
module Lib.Day10 (
    run
) where

import Text.Regex.TDFA ((=~))
import Lib.Util
import Data.Maybe (isJust)
import Data.List (find)
import qualified Data.Set as S

data Point = Point {
    x, y, vx, vy :: Int
} deriving (Show, Eq)

readPoints :: String -> Point
readPoints line =
    let x:y:vx:vy:_ = map read $ tail $ head $ line =~ "position=< *(-?[0-9]+), *(-?[0-9]+)> velocity=< *(-?[0-9]+), *(-?[0-9]+)>"
    in Point{x, y, vx, vy}

adjacent (x, y) = [
    (x + dx, y + dy) | dx <- [-1..1],
                       dy <- [-1..1],
                       not (dx == dy && dx == 0)]

move (n, m) = (n + 1, foldr (\Point{x, y, vx, vy} ps -> Point{x = x + vx, y = y + vy, vx, vy}:ps) [] m)

hasLetters (_, m) = 
    let idxs = S.fromList $ map (\Point{x, y} -> (x, y)) m
    in all (\Point{x, y} -> any (`S.member` idxs) (adjacent (x, y))) m

printMat m =
    let ((xmin, xmax), (ymin, ymax)) = getBounds m
    in unlines $ map (\y -> map (\x -> go (x, y)) [xmin..xmax]) [ymin..ymax]
    where go (x', y') = if isJust $ find (\Point{x, y} -> x == x' && y == y') m then '#' else '.'

getBounds :: [Point] -> ((Int, Int), (Int, Int))
getBounds ps =
    let xs = map x ps
        ys = map y ps
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
    in ((minX, maxX), (minY, maxY))

run = do
    f <- readFile "input\\day10"
    let points = map readPoints $ lines f
    print $ snd $ until hasLetters move (0, points)