module Lib.Day12 where

import Data.List (find)
import Lib.Util
import Data.Function (on)

data Rule = Rule { pat :: [Bool], next :: Bool }

readRule :: String -> Rule
readRule line =
    let pat:next:_ = getMatches "([.#]*) => ([.#])" line
    in Rule{pat = map (== '#') pat, next = next == "#"}

groupPots :: [(Int, Bool)] -> [(Int, [Bool])]
groupPots pots =
    let n = fst $ head pots
        m = fst $ last pots
    in scanl (\(i, _:acc) (_, p) -> (i + 1, acc ++ [p])) (n-3, replicate 5 False) $ pots ++ zip [m+1..m+5] (repeat False)

evolvePlant :: [Rule] -> [Bool] -> Bool
evolvePlant rules potGroup = maybe False next $ find ((== potGroup) . pat) rules

proliferate :: [Rule] -> [(Int, Bool)] -> [(Int, Bool)]
proliferate rules pots = 
    let potGroups = groupPots pots
    in dropBackWhile (not . snd) $ dropWhile (not . snd) $ map (mapSnd (evolvePlant rules)) potGroups

isStable (_, prev, curr) = map snd prev == map snd curr

part1 rules = doTimes 20 (proliferate rules)

part2 rules pots =
    let (n, _, curr) = until isStable (\(n, _, curr) -> (n + 1, curr, proliferate rules curr)) (0, [], pots)
    in map (\(i, p) -> (i + (50000000000 - n), p)) curr

run = do
    lines <- lines <$> readFile "input\\day12"
    let pots = zip [0..] $ map (== '#') $ last $ words $ head lines
    let rules = map readRule $ drop 2 lines
    let final = part2 rules pots
    print $ sum $ map fst $ filter snd final