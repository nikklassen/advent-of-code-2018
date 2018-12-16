{-# LANGUAGE FlexibleContexts #-}
module Lib.Day11 where

import Lib.Matrix
import Lib.Util
import Data.Maybe (catMaybes, isJust)
import Data.List (maximumBy)
import Data.Ord (comparing)

serial = 3031

getPower x y =
    let rID = x + 11
        pow = (rID * (y + 1) + serial) * rID
    in ((pow `div` 100) `rem` 10) - 5

powForBox g x y size = sum [g ! (x, y) | x <- [x .. x + size-1] :: [Int], y <- [y .. y + size-1] :: [Int]]

moveBox :: Matrix Int -> Int -> Int -> Int -> Int -> Maybe (Int, Int, Int)
moveBox g x y size pow
    | x + size < 300 =
        let pow' = foldl (\pow j -> pow - (g ! (x, j)) + (g ! (x+size, j))) pow ([y .. (y + size - 1)] :: [Int])
        in Just (x + 1, y, pow')
    | y + size < 300 =
        let x' = 0
            y' = y + 1
        in Just (x', y', powForBox g x' y' size)
    | otherwise = Nothing

getMaxForSize :: Matrix Int -> Int -> (Int, Int, Int)
getMaxForSize g size = 
    let x = 0
        y = 0
        pow = powForBox g x y size
    in maximumBy (comparing thd) $ catMaybes $ takeWhile isJust $ iterate (\i -> i >>= (\(x, y, pow) -> moveBox g x y size pow)) $ Just (x, y, pow)

maxBoxes sizes =
    let g = Lib.Matrix.fromList [((x, y), getPower x y) | x <- [0..299] :: [Int], y <- [0..299] :: [Int]]
    in maximumBy (comparing (thd . snd)) $ map (\size -> (size, getMaxForSize g size)) sizes

part1 = do
    let (_, (x, y, _)) = maxBoxes [3]
    putStrLn $ show (x + 1) ++ "," ++ show (y + 1)

part2 g = do
    let (size, (x, y, _)) = maxBoxes [1..299]
    putStrLn $ show (x + 1) ++ "," ++ show (y + 1) ++ "," ++ show size

run = part1