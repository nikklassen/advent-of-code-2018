module Lib.Day13 where

import Prelude hiding (lookup)
import Control.Lens
import Control.Applicative
import Lib.Matrix
import Data.Function (on)
import Data.List (concat)
import Data.Maybe (isJust, fromJust)
import Data.Set (Set)
import qualified Data.Set as S

data TurnDirection = TLeft | LeftStraight | TRight deriving (Show, Eq)

data Cart = Cart {
    x, y :: Int,
    dir :: (Int, Int),
    td :: TurnDirection
} deriving (Show)

instance Coord Cart where
    toCoord Cart{x, y} = (x, y)

instance Eq Cart where
    (==) = (==) `on` toCoord

instance Ord Cart where
    compare Cart{x = x1, y = y1} Cart{x = x2, y = y2} = if y1 == y2 then x1 `compare` x2 else y1 `compare` y2

extractCart d (x, y) =
    let dir = case d of
                '>' -> (1, 0)
                '^' -> (0, -1)
                '<' -> (-1, 0)
                'v' -> (0, 1)
                _ -> undefined
    in Cart{x, y, dir, td = TLeft}

replaceTrack :: Matrix Char -> (Int, Int) -> Char
replaceTrack g (x, y)
    | up && down && left && right = '+'
    | up && down = '|'
    | left && right = '-'
    | up && right || down && left = '\\'
    | down && right || up && left = '/'
    | otherwise = undefined
    where isTrack ts (x', y') = maybe False (`elem` ts) $ (x', y') `lookup` g
          up = isTrack "|+/\\/" (x, y - 1)
          down = isTrack "|+\\/" (x, y + 1)
          left = isTrack "+-\\/" (x - 1, y)
          right = isTrack "+-\\/" (x + 1, y)

turn d TLeft = (rotate d 'L', LeftStraight)
turn d LeftStraight = (d, TRight)
turn d TRight = (rotate d 'R', TLeft)

rotate (x, y) 'R' = (-y, x)
rotate (x, y) 'L' = (y, -x)
rotate _ _ = undefined

updateCart Cart{x, y, dir = dir@(dx, dy), td} track =
    let (dir'@(dx', dy'), td')
          | track `elem` "|-" = (dir, td)
          | track == '+' = turn dir td
          | track == '/' && dy /= 0 || track == '\\' && dx /= 0 = (rotate dir 'R', td)
          | track == '\\' && dy /= 0 || track == '/' && dx /= 0 = (rotate dir 'L', td)
          | otherwise = undefined
    in Cart{x = x + dx', y = y + dy', dir = dir', td = td'}

printTrack :: Matrix Char -> [Cart] -> String
printTrack g carts = printMatrix $ updateTrack g carts

updateTrack :: Matrix Char -> [Cart] -> Matrix Char
updateTrack = foldl (\g c -> setElem c (showCart c) g)
    where showCart Cart{dir} =
            case dir of
                (0, -1) -> '^'
                (0, 1) -> 'v'
                (1, 0) -> '>'
                (-1, 0) -> '<'
                _ -> undefined

move :: Matrix Char -> Set Cart -> (Maybe Cart, Set Cart)
move grid carts = _2 %~ (^._2) $ until (view (_2._1.to S.null)) move' (Nothing, (carts, S.empty))
    where move' (destroyed, (oldCarts, newCarts)) =
            let (c, oldCarts') = S.deleteFindMin oldCarts
                newCart = updateCart c (grid ! c)
            in if newCart `S.member` newCarts then
                   (destroyed <|> Just newCart, (oldCarts', newCart `S.delete` newCarts))
               else if newCart `S.member` oldCarts' then
                   (destroyed <|> Just newCart, (newCart `S.delete` oldCarts', newCarts))
               else
                   (destroyed, (oldCarts', newCart `S.insert` newCarts))

findCollision :: Matrix Char -> [Cart] -> (Cart, Set Cart)
findCollision grid carts =
        (_1 %~ fromJust) . view _2 $
        until (^._2._1.to isJust)
              (\(n, (_, cs)) -> (n + 1, move grid cs))
              (0 :: Int, (Nothing, S.fromList carts))

part1 carts grid = toCoord $ fst $ findCollision grid carts

part2 grid = part2'
    where part2' carts =
            let (_, newCarts) = findCollision grid carts
            in if S.size newCarts == 1 then toCoord $ S.findMin newCarts
               else part2' (S.elems newCarts)

run = do
    ls <- lines <$> readFile "input\\day13"
    let is = [0..] :: [Int]
    let grid = Lib.Matrix.fromList $ concat $ zipWith (\y line -> zipWith (\x c -> ((x, y), c)) is line) is d
    let (carts, tracks) = foldMatrixWithKey
            (\(cs, t) p c ->
                if c `elem` "><^v" then
                    (extractCart c p:cs, setElem p (replaceTrack t p) t)
                else (cs, t))
            ([], grid)
            grid
    -- print $ part1 carts noCarts
    print $ part2 tracks carts