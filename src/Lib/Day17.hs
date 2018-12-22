{-# LANGUAGE RankNTypes #-}
module Lib.Day17 where

import Data.Maybe (fromJust, fromMaybe)
import Linear (V2(..), _x, _y)
import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Lib.Util

data Ground = Clay | Sand | Resting | Flowing | Source deriving (Eq)

instance Show Ground where
    show Clay = "#"
    show Sand = "."
    show Resting = "~"
    show Flowing = "|"
    show Source = "+"

type Point = V2 Int

data Square = Square {
    _ground :: Ground,
    _pt :: Point
} deriving (Show, Eq)

makeLenses ''Square

type Slice = Map Point Ground

parseLine :: String -> [(Point, Ground)]
parseLine line =
    let c1:a1:b1:b2:_ = "([xy])=([0-9]+), [xy]=([0-9]+)\\.\\.([0-9]+)" `getMatches` line
        a = [read a1 :: Int]
        b = [(read b1 :: Int)..(read b2 :: Int)]
    in if c1 == "x" then
         [(V2 x y, Clay) | x <- a, y <- b]
       else 
         [(V2 x y, Clay) | x <- b, y <- a]


getBounds :: [Point] -> ((Int, Int), (Int, Int))
getBounds points =
    let xMin = minimum1Of (folded._x) points
        xMax = maximum1Of (folded._x) points
        yMin = minimum1Of (folded._y) points
        yMax = maximum1Of (folded._y) points
    in ((xMin, xMax), (yMin, yMax))

printSlice :: Slice -> String
printSlice s =
    let ((xMin, xMax), (yMin, yMax)) = getBounds $ M.keys s
        points = [[getGround s (V2 x y) | x <- [xMin..xMax]] | y <- [yMin..yMax]]
    in unlines $ map (concatMap show) points

down = _y +~ 1
left = _x -~ 1
right = _x +~ 1

getGround s p = M.findWithDefault Sand p s

flow :: ((Int, Int), (Int, Int)) -> Slice -> Maybe Slice
flow ((xMin, xMax), (_, yMax)) !s = M.foldlWithKey go Nothing s
    where go :: Maybe Slice -> Point -> Ground -> Maybe Slice
          go !new p g
            | p^._y > yMax = new
            | g `elem` [Flowing, Source] =
                if d == Sand then
                    Just $ M.insert (down p) Flowing $ fromMaybe s new
                else if d /= Flowing then
                    flowHor xMin xMax p s new
                else
                    new
            | otherwise = new
            where d = getGround s (down p)
                  l = getGround s (left p)
                  r = getGround s (right p)

hasEdge dir p slice
    | getGround slice (down p) == Sand = True
    | g `elem` [Sand, Flowing] = hasEdge dir (dir p) slice
    | otherwise = False
    where g = getGround slice (dir p)

hasWall xMin xMax dir p slice
    | g `elem` [Clay, Resting] = True
    | g `elem` [Sand, Flowing] = hasWall xMin xMax dir (dir p) slice
    | otherwise = False
    where g = getGround slice (dir p)

flowHor xMin xMax p slice new
    | hasEdge left p slice || hasEdge right p slice = fillWith Flowing
    | hasWall xMin xMax left p slice && hasWall xMin xMax right p slice = fillWith Resting
    | otherwise = new
    where insertIfChanged p newG new = if newG == getGround slice p then new else Just $ M.insert p newG $ fromMaybe slice new
          fillWith newG = new & flowDir' left p newG & flowDir' right p newG & insertIfChanged p newG
          flowDir' dir p newG new =
                let g = getGround slice (dir p)
                    new' = insertIfChanged p newG new
                in if g == Sand && (newG /= Flowing || getGround slice (down p) /= Sand) then
                        flowDir' dir (dir p) newG new'
                    else
                        new'
    
fix' f a = let a' = f a in maybe a (fix' f) a'

countSquares types yMin yMax = length . filter (\(V2 _ y, g) -> yMin <= y && y <= yMax && g `elem` types) . M.assocs 

run types = do
    f <- readFile "input\\day17"
    let points = concatMap parseLine $ lines f
    let bs@(_, (yMin, yMax)) = getBounds $ points^..folded._1
    let s = M.fromList $ (V2 500 0, Source):points
    print $ countWater types yMin yMax $ fix' (flow bs) s

part1 = run [Source, Flowing, Resting]

part2 = run [Resting]