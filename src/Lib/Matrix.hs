module Lib.Matrix (
    Matrix,
    updateWithDefault,
    foldMatrix,
    empty,
    setElem,
    (!),
    fromList,
    mapMatrixWithKey,
    elems,
    lookup,
    member
) where

import Prelude hiding (lookup)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Control.Applicative ((<|>))

newtype Matrix a = Matrix (IntMap (IntMap a))
    deriving (Show)

updateWithDefault :: (a -> a) -> (Int, Int) -> a -> Matrix a -> Matrix a
updateWithDefault f (x, y) d (Matrix m)=
    let updateWithDefault' f d = fmap f . (<|> Just d)
    in Matrix $ M.alter (updateWithDefault' (M.alter (updateWithDefault' f d) y) M.empty) x m

foldMatrix :: (a -> b -> a) -> a -> Matrix b -> a
foldMatrix f z (Matrix m) = M.foldl (M.foldl f) z m

foldMatrixWithKey :: (a -> (Int, Int) -> b -> a) -> a -> Matrix b -> a
foldMatrixWithKey f z (Matrix m) = M.foldlWithKey (\acc x m -> M.foldlWithKey (\d y v -> f d (x, y) v) acc m) z m

mapMatrixWithKey :: ((Int, Int) -> a -> b) -> Matrix a -> Matrix b
mapMatrixWithKey f (Matrix m) = Matrix $ M.mapWithKey (\x m -> M.mapWithKey (\y d -> f (x, y) d) m) m

empty = Matrix M.empty

(!) (Matrix m) (x, y) = (m M.! x) M.! y

lookup :: (Int, Int) -> Matrix a -> Maybe a
lookup (x, y) (Matrix m) = M.lookup x m >>= M.lookup y

setElem :: (Int, Int) -> a -> Matrix a -> Matrix a
setElem (x, y) e (Matrix m) =
    let outer = if x `M.member` m then m M.! x else M.empty
        outer' = M.insert y e outer
    in Matrix $ M.insert x outer' m

fromList :: [((Int, Int), a)] -> Matrix a
fromList = foldl (\m (idx, e) -> setElem idx e m) empty

member :: (Int, Int) -> Matrix a -> Bool
member (x, y) (Matrix m) = x `M.member` m && y `M.member` (m M.! x)

elems = foldMatrixWithKey (\acc idx e -> (idx, e):acc) []