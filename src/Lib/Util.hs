module Lib.Util where

import Text.Regex.TDFA ((=~))
import qualified Data.List as L
import Data.Function (on)
import Debug.Trace (trace)

groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy comp = map (\a -> (comp $ head a, a)) . L.groupBy ((==) `on` comp) . L.sortOn comp

countElemsBy :: (a -> Bool) -> [a] -> Int
countElemsBy pred = foldl (\acc i -> if pred i then acc + 1 else acc) 0

accumBy :: (a -> Int) -> [a] -> Int
accumBy f = foldl (\acc i -> acc + f i) 0

trace1 :: (Show a) => a -> a
trace1 a = trace (show a) a

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

thd :: (a, b, c) -> c
thd (_, _, c) = c

(!!?) :: [a] -> Int -> Maybe a
(!!?) xs i = if i < length xs then Just (xs !! i) else Nothing

getMatches :: String -> String -> [String]
getMatches p s = tail $ head $ s =~ p

getMatchesAs :: Read a => String -> String -> [a]
getMatchesAs p s = map read $ tail $ head $ s =~ p

doTimes :: (Eq n, Num n) => n -> (a -> a) -> a -> a
doTimes 0 f x = x
doTimes n f x  = doTimes (n - 1) f (f x)

dropBackWhile f [] = []
dropBackWhile f [a] = if f a then [] else [a]
dropBackWhile f (a:as) = let bs = dropBackWhile f as in if null bs && f a then [] else a:bs