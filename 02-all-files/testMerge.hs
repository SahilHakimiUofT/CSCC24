{-
How to use: runghc testMerge.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import TestLib
import Text.Read (readMaybe)

import qualified Merge

-- Re-assert desired type.
merge :: [Integer] -> [Integer] -> [Integer]
merge = Merge.merge

tests = [ "handout" ~: merge [2, 3, 5] [1, 3, 4, 4, 7] ~?= [1, 2, 3, 3, 4, 4, 5, 7]
          -- More test cases when marking.
        ]

main = testlibMain tests
