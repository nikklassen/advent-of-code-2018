{-# LANGUAGE DoAndIfThenElse, NamedFieldPuns #-}
module Lib.Day1 where

import Data.String (lines, words)

data State = State
        { dir :: (Int, Int)
        , x :: Int
        , y :: Int
        }

rotate (x, y) 'R' = (y, -x)
rotate (x, y) 'L' = (-y, x)

walk :: State -> String -> State
walk State{dir, x, y } (rot:len) =
        let dist = (read len :: Int)
            dir'@(dx, dy) = rotate dir rot
            x' = x + (dist * dx)
            y' = y + (dist * dy)
        in State { x=x', y=y', dir=dir' }

dist State{x, y} = abs x + abs y

run :: IO Int
run = do
    content <- readFile "input\\day01"
    let instrs = map (filter (/= ',')) $ words content
    return $ dist $ foldl walk State{ x=0, y=0, dir=(0, 1) } instrs
