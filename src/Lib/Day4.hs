{-# LANGUAGE NamedFieldPuns #-}
module Lib.Day4 (
    run
) where

import Text.Regex.TDFA ((=~))
import Data.List (sort, maximumBy)
import Lib.Util (groupBy, countElemsBy, accumBy)
import Data.Ord (comparing)

data Shift = Shift {
    gid :: Int,
    day :: (Int, Int),
    sleep :: [(Int, Int)]
} deriving (Show)

readTimes :: String -> ((Int, Int), (Int, Int), String)
readTimes line =
    let m:d:h:min:s:_ = tail $ head $ line =~ "\\[1518-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\] (.*)"
    in ((read m, read d), (read h, read min), s)

getShifts :: [((Int, Int), (Int, Int), String)] -> [Shift]
getShifts (t:ts) = reverse $ foldl go (makeShift t []) ts
    where makeShift :: ((Int, Int), (Int, Int), String) -> [Shift] -> [Shift]
          makeShift (day, time, obs) prev =
              let gid = obs =~ "Guard #([0-9]+)" :: [[String]]
              in if not (null gid) then
                     Shift{gid = read $ head gid !! 1, day, sleep = []}:prev
                 else
                     let [Shift {gid, day, sleep }] = prev
                     in if obs =~ "wakes up" then
                           let (th, (s, _)) = (init sleep, last sleep)
                           in [Shift {gid, day, sleep = th ++ [(s, snd time)]}]
                        else 
                           [Shift {gid, day, sleep = sleep ++ [(snd time, 0)]}]
          go (s:ss) t = makeShift t [s] ++ ss

findSleepiest :: ((Int, Int, Int, Int) -> Int) -> [Shift] -> Int
findSleepiest f shifts = let (g, _, m, _) = maximumBy (comparing f) $ map countSleeps $ groupBy gid shifts in g * m
    where countSleeps (g, shifts) =
            let sleeps = foldl (\acc s -> acc ++ sleep s) [] shifts
                totalSleeps = accumBy (\(a, b) -> b - a) sleeps
                (sleepiestMin, freq) = maximumBy (comparing snd) $ map (\i -> (i, countElemsBy (\(a, b) -> i >= a && i < b) sleeps)) [0..59]
            in (g, totalSleeps, sleepiestMin, freq)

run1 = findSleepiest (\(_, s, _, _) -> s)

run2 = findSleepiest (\(_, _, _, freq) -> freq)

run = do
    f <- readFile "input\\day04"
    let shifts = getShifts $ sort $ map readTimes $ lines f
    print $ run2 shifts