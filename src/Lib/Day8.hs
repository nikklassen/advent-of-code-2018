{-# LANGUAGE NamedFieldPuns #-}
module Lib.Day8 (
    run
) where

import Lib.Util (mapFst, mapSnd, (!!?), trace1)
import Data.Maybe (fromMaybe)

data Node = Node {
    meta :: [Int],
    children :: [Node]
} deriving (Show)

readChildren 0 nums = ([], nums)
readChildren n nums =
    let (node, remaining) = readTree nums
    in mapFst ((:) node) $ readChildren (n - 1) remaining

readTree (c:m:nums) =
    let (children, (meta, remaining)) = mapSnd (splitAt m) $ readChildren c nums
    in (Node{meta, children}, remaining)

sumMeta n = sum (meta n) + sum (map sumMeta (children n))

findValue Node{meta, children = []} = sum meta
findValue Node{meta, children} =
    let values = map findValue children
    in sum $ map (\m -> fromMaybe 0 $ values !!? (m - 1)) meta

run = do
    f <- readFile "input\\day08"
    let nums = map read $ words f
    print $ findValue $ fst $ readTree nums