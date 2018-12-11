{-# LANGUAGE TupleSections #-}
module Lib.Day6 (
    run
) where

import Lib.Matrix (Matrix, (!), fromList, setElem, foldMatrix)
import Data.List (findIndex)
import Data.Maybe (mapMaybe)
import Lib.Util (mapSnd)
import qualified Data.IntMap.Lazy as M

data FillState = Empty | DMZ | Filled Int deriving (Show, Eq)

type Fringe = [((Int, Int), FillState)]

getBounds :: [(Int, Int)] -> ((Int, Int), (Int, Int))
getBounds = getBounds' ((maxBound, minBound), (maxBound, minBound))
    where getBounds' ((xmin, xmax), (ymin, ymax)) ((x, y):ps) = getBounds' ((min x xmin, max x xmax), (min y ymin, max y ymax)) ps
          getBounds' bs [] = bs

adjacent (x, y) (xbs, ybs) = filter (\(x, y) -> inRange x xbs && inRange y ybs) $ map (\(dx, dy) -> (x + dx, y + dy)) [(-1, 0), (1, 0), (0, 1), (0, -1)]

inRange x (xMin, xMax) = x >= xMin && x <= xMax

addToFringe :: Fringe -> ((Int, Int), FillState) -> Fringe
addToFringe fringe (x, f) =
    case findIndex ((== x) . fst) fringe of
        Nothing -> (x, f):fringe
        Just i ->
            if snd (fringe !! i) == f then
                fringe
            else
               let (ys, _:zs) = splitAt i fringe in ys ++ (x, DMZ):zs

proliferate' :: ((Int, Int), (Int, Int)) -> (Fringe, Matrix FillState) -> (Fringe, Matrix FillState)
proliferate' bs (fringe, m) =
    let unfilled p = filter ((== Empty) . (!) m) $ adjacent p bs
        fringe' = foldl addToFringe [] $ concatMap (\(p, f) -> map (, f) $ unfilled p) $ filter (\(_, f) -> isFilled f) fringe
        m' = foldl (\m (p, f) -> setElem p f m) m fringe'
    in (filter (isFilled . snd) fringe', m')

isFilled (Filled _) = True
isFilled _ = False

findBiggest :: Matrix FillState -> Int
findBiggest = maximum . M.elems . foldMatrix go M.empty
    where go acc (Filled i) = M.insert i (1 + M.findWithDefault 0 i acc) acc
          go acc _ = acc

distanceToCenter (x, y) (cx, cy) = abs (cx - x) + abs (cy - y)

getGrid = do
    f <- readFile "input\\day06"
    let points = map (\p -> read $ "(" ++ p ++ ")") $ lines f :: [(Int, Int)]
    return (getBounds points, points)

part1 = do
    (bs@((minX, maxX), (minY, maxY)), points) <- getGrid
    let mInit = fromList [((x, y), Empty) | x <- [minX..maxX], y <- [minY..maxY]]
    let centers = zip points $ map Filled [1..]
    let m = foldl (\acc (idx, c) -> setElem idx c acc) mInit centers
    print $ findBiggest $ snd $ until (null . fst) (proliferate' bs) (centers, m)

part2 = do
    (((minX, maxX), (minY, maxY)), points) <- getGrid
    print $ length $ filter (\p -> sum (map (distanceToCenter p) points) < 10000) [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]

run = part1