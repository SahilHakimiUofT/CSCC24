{-
How to use: runghc testSplit.hs
-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import TestLib
import Text.Read (readMaybe)

import qualified Split

-- Re-assert desired type.
split :: [a] -> ([a], [a])
split = Split.split

tests = [ "handout even" ~: split [3,1,2,9] ~?= ([3,2], [1,9])
        , "handout odd" ~: split [3,1,2,9,7] ~?= ([3,2,7], [1,9])
        , "handout singleton" ~: split [3] ~?= ([3], [])
          -- More test cases when marking.
        ]

main = testlibMain tests
