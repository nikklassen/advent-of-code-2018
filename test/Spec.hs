import qualified Lib.Zipper.Spec as Z

import System.Exit

main :: IO ()
main = do
  -- add test runners into the array for each module
  good <- and <$> sequence [Z.runTests]
  if good
     then exitSuccess
     else exitFailure