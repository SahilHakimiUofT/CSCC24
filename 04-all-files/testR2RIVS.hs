import RIVSDef
import R2RIVS
import TestLib

tests =
    [ "plus" ~: plus (MkR2 3 1) (MkR2 2 (-5)) ~?= MkR2 5 (-4)
    , "minus" ~: minus (MkR2 5 (-4)) (MkR2 2 (-5)) ~?= MkR2 3 1
    , "scale" ~: scale (-0.5) (MkR2 2 (-4)) ~?= MkR2 (-1) 2
    , "dot" ~: dot (MkR2 7 (-3)) (MkR2 (-2) (-4)) ~?= -2
    ]
-- more when marking

main = testlibMain tests
