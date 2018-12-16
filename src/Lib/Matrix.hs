{-# LANGUAGE FlexibleInstances #-}
module Lib.Matrix (
    Matrix,
    Coord(..),
    updateWithDefault,
    foldMatrix,
    empty,
    setElem,
    (!),
    fromList,
    mapMatrixWithKey,
    foldMatrixWithKey,
    elems,
    lookup,
    member,
    printMatrix,
    printMatrixBy,
) where

import Prelude hiding (lookup)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Control.Applicative ((<|>))

newtype Matrix a = Matrix (IntMap (IntMap a))
    deriving (Show)

class Coord c where
    toCoord :: c -> (Int, Int)

instance Coord (Int, Int) where
    toCoord = id

updateWithDefault :: Coord c => (a -> a) -> c -> a -> Matrix a -> Matrix a
updateWithDefault f c d (Matrix m)=
    let (x, y) = toCoord c
        updateWithDefault' f d = fmap f . (<|> Just d)
    in Matrix $ M.alter (updateWithDefault' (M.alter (updateWithDefault' f d) y) M.empty) x m

foldMatrix :: (a -> b -> a) -> a -> Matrix b -> a
foldMatrix f z (Matrix m) = M.foldl (M.foldl f) z m

foldMatrixWithKey :: (a -> (Int, Int) -> b -> a) -> a -> Matrix b -> a
foldMatrixWithKey f z (Matrix m) = M.foldlWithKey (\acc x m -> M.foldlWithKey (\d y v -> f d (x, y) v) acc m) z m

mapMatrixWithKey :: ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
mapMatrixWithKey f (Matrix m) = Matrix $ M.mapWithKey (\x m -> M.mapWithKey (\y d -> f (x, y) d) m) m

empty = Matrix M.empty

(!) :: Coord c => Matrix a -> c -> a
(!) (Matrix m) c = let (x, y) = toCoord c in (m M.! x) M.! y

lookup :: Coord c => c -> Matrix a -> Maybe a
lookup c (Matrix m) = let (x, y) = toCoord c in M.lookup x m >>= M.lookup y

setElem :: Coord c => c -> a -> Matrix a -> Matrix a
setElem c e (Matrix m) =
    let (x, y) = toCoord c
        outer = if x `M.member` m then m M.! x else M.empty
        outer' = M.insert y e outer
    in Matrix $ M.insert x outer' m

fromList :: Coord c => [(c, a)] -> Matrix a
fromList = foldl (\m (idx, e) -> setElem idx e m) empty

member :: Coord c => c -> Matrix a -> Bool
member c (Matrix m) = let (x, y) = toCoord c in x `M.member` m && y `M.member` (m M.! x)

elems = foldMatrixWithKey (\acc idx e -> (idx, e):acc) []

getBounds :: Matrix a -> ((Int, Int), (Int, Int))
getBounds m =
    let ps = map fst $ elems m
        xs = map fst ps
        ys = map snd ps
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
    in ((minX, maxX), (minY, maxY))

printMatrix :: Matrix Char -> String
printMatrix = printMatrixBy id

printMatrixBy :: (a -> Char) -> Matrix a -> String
printMatrixBy f m =
    let ((xmin, xmax), (ymin, ymax)) = getBounds m
    in unlines $ map (\y -> map (\x -> f $ m ! (x, y)) [xmin..xmax]) [ymin..ymax]