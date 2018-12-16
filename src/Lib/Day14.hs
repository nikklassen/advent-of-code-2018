{-# LANGUAGE BangPatterns #-}
module Lib.Day14 where

import Data.Sequence (Seq, index)
import qualified Data.Sequence as S
import Lib.Util
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)

digits n = 
    let t = [n `div` 10 | n > 9]
    in S.fromList $ t ++ [n `rem` 10]

placeMarble :: (Seq Int, Int, Int) -> (Seq Int, Int, Int)
placeMarble (!r, c1, c2) =
    let s1 = r `index` c1
        s2 = r `index` c2
        r' = r <> digits (s1 + s2)
    in (r', wrap (c1 + s1 + 1) r', wrap (c2 + s2 + 1) r')
    where wrap n r = n `rem` S.length r

hasLength x (scores, _, _) = S.length scores >= x

slice :: Int -> Int -> Seq a -> [a]
slice n m s = toList $ S.take (m - n) $ snd $ S.splitAt n s

getScores (s, _, _) = s

getPositionOfDigits ds z =
    let nz = S.length z
        nd = length ds
        t = toList $ snd $ S.splitAt (nz - nd - 2) z
    in if ds `isPrefixOf` t then
            Just $ nz - nd
       else if ds `isPrefixOf` tail t then
            Just $ nz - nd - 1
       else
            Nothing

part1 = do
    let n = 919901
    putStrLn $ concatMap show $ slice n (n + 10) $ (\(s, _, _) -> s) $ until (hasLength (n + 10)) placeMarble (S.fromList [3, 7], 0, 1)

part2 = do
    let ds = [9,1,9,9,0,1]
    print $ head $ mapMaybe (\(z, _, _) -> getPositionOfDigits ds z) $ iterate placeMarble (S.fromList [3, 7], 0, 1)

run = part2