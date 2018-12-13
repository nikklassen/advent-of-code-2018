{-# LANGUAGE BangPatterns #-}
module Lib.Day9 where

import Lib.Util (doTimes)
import Lib.Zipper (Zipper)
import qualified Lib.Zipper as Z

posMod :: Integral i => i -> i -> i
posMod x n = let m = x `rem` n in if m < 0 then n + m else m

placeMarble :: (Int, Zipper Int, Zipper Int) -> (Int, Zipper Int, Zipper Int)
placeMarble (n, !marbles, !scores) =
    if n `rem` 23 == 0 then
       let m1 = doTimes 7 Z.leftWrap marbles
           z = Z.cursor m1
           marbles' = Z.delete m1
           scores' = Z.rightWrap $ Z.replace (n + z + Z.cursor scores) scores
       in (n + 1, marbles', scores')
    else
      let marbles' = Z.insert n $ doTimes 2 Z.rightWrap marbles
      in (n + 1, marbles', Z.rightWrap scores)

isRoundN x (n, _, _) = x == n + 1

maxScore (_, _, scores) = maximum $ Z.toList scores

run = do
    let players = 459
    let n = 7179000
    print $ maxScore $ until (isRoundN n) placeMarble (1, Z.fromList [0], Z.fromList $ replicate players 0)