{-# LANGUAGE NamedFieldPuns, LambdaCase #-}
module Lib.Day6 (
    run
) where

import Prelude hiding (lookup)
import Data.Ord (comparing)
import Lib.Matrix (Matrix, mapMatrixWithKey, (!), fromList, setElem, elems, foldMatrix, lookup)
import Data.List (minimumBy, maximumBy, findIndex)
import Data.Maybe (mapMaybe)
import Lib.Util (trace1, mapSnd)
import qualified Data.IntMap.Lazy as M

data FillState = Empty | Center Int | DMZ | Filled Int
    deriving (Show)

type Fringe = [((Int, Int), FillState)]

getBounds :: [(Int, Int)] -> ((Int, Int), (Int, Int))
getBounds ps =
    let xs = map fst ps
        ys = map snd ps
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
    in ((minX, maxX), (minY, maxY))

filterInf ((minX, maxX), (minY, maxY)) =
    filter (\(x, y) -> x > minX && x < maxX && y > minY && y < maxY)

adjacent (x, y) (xbs, ybs) = [
    (x + dx, y + dy) | dx <- [-1..1],
                       dy <- [-1..1],
                       not (dx == dy && dx == 0) && inRange (x + dx) xbs && inRange (y + dy) ybs]

inRange x (xMin, xMax) = x >= xMin && x <= xMax

addToFringe :: Fringe -> ((Int, Int), FillState) -> Fringe
addToFringe fringe (x, f) =
    case findIndex ((==) x . fst) fringe of
        Just i -> let (ys, _:zs) = splitAt i fringe in ys ++ (x, DMZ):zs
        Nothing -> (x, f):fringe

fill (Center c) = Just $ Filled c
fill (Filled c) = Just $ Filled c
fill DMZ = Nothing
fill Empty = Nothing

proliferate' :: ((Int, Int), (Int, Int)) -> (Fringe, Matrix FillState) -> (Fringe, Matrix FillState)
proliferate' bs (fringe, m) =
    let unfilled p = filter (isEmpty . (!) m) $ adjacent p bs
        fringe' = foldl addToFringe [] $ concatMap (\(p, f) -> mapMaybe (\p -> (,) p <$> fill f) (unfilled p)) fringe
        m' = foldl (\m (p, f) -> setElem p f m) m fringe'
    in (filter (isFilled . snd) fringe', m')

isFilled (Center c) = True
isFilled (Filled c) = True
isFilled _ = False

isEmpty Empty = True
isEmpty _ = False

getKey (Center c) = c
getKey (Filled c) = c
getKey _ = 0

findBiggest :: Matrix FillState -> Int
findBiggest = maximum . M.elems . foldMatrix go M.empty
    where go acc s =
            let k = getKey s
            in if k == 0 then
                acc
            else
                M.insert k (M.findWithDefault 0 k acc) acc

printMat m =
    unlines $ map (\x -> unwords $ map (\y -> go (x, y)) [1..9]) [1..9]
    where go idx =
            let k = maybe 0 getKey $ lookup idx m
            in if k == 0 then "." else show k

run = do
    f <- readFile "input\\day06_test"
    let points = map (\p -> read $ "(" ++ p ++ ")") $ lines f :: [(Int, Int)]
    let bs@((minX, maxX), (minY, maxY)) = getBounds points
    let mInit = fromList [((x, y), Empty) | x <- [minX..maxX], y <- [minY..maxY]]
    let m = foldl (\acc (i, idx) -> setElem idx (Center i) acc) mInit $ zip [1..] points
    -- print $ until (all notEmpty . map snd . elems) (proliferate bs) m
    putStrLn $ printMat $ snd $ until (null . fst) (proliferate' bs) (map (mapSnd Center) (zip points [1..]), m)
