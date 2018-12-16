module Lib.Day2 where

import Data.Map (Map)
import qualified Data.Map as M

freqs w = M.fromListWith (+) $ map (,1) w

countN :: Int -> [Map k Int] -> Int
countN n = length . filter (elem n)

part1 strs = 
    let cFreqs = map freqs strs
        num2 = countN 2 cFreqs
        num3 = countN 3 cFreqs
    in num2 * num3

isSimilar (s1, s2) =
    let diff = take 2 $ filter (uncurry (/=)) $ zip s1 s2
    in length diff == 1

part2 strs =
    let (s1', s2') = head $ filter isSimilar [(s1, s2) | s1 <- strs, s2 <- strs]
    in map fst $ filter (uncurry (==)) $ zip s1' s2'

run = do
    strs <- lines <$> readFile "input\\day02"
    -- print $ part1 strs
    print $ part2 strs