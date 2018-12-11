{-# LANGUAGE NamedFieldPuns #-}
module Lib.Day5 (
    run
) where

import Data.Char (toUpper, toLower)

eqNoCase c1 c2 = toUpper c1 == c2 || toLower c1 == c2

react s = foldr go [last s] $ init s 
    where go c2 (c:cs) = if c /= c2 && eqNoCase c c2 then cs else c2:c:cs
          go c2 [] = [c2]

fix f s = let s2 = f s in if s == s2 then s else fix f s2

filterChar c = filter (not . eqNoCase c)

run = do
    f <- readFile "input\\day05"
    print $ minimum $ map (\c -> length $ fix react $ filterChar c f) ['a'..'z']