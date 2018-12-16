{-# LANGUAGE TemplateHaskell #-}
module Lib.Zipper.Spec where

import Lib.Zipper (Zipper)
import qualified Lib.Zipper as Z
import Test.QuickCheck

prop_pushBack :: Gen Bool
prop_pushBack = do
    (z1, l1) <- arbitrary :: Gen (Zipper Int, [Int])
    return $ Z.toList (pushBack l1 z1) == (Z.toList z1 ++ l1)

prop_setCursor :: Zipper Int -> NonNegative Int -> Property
prop_setCursor z (NonNegative n) = n < Z.length z ==>
    Z.cursor (setCursor n z) == Z.toList z !! n

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll