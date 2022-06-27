module R2RIVS where

import RIVSDef

-- ℝ² is a well known real inner-product vector space, and is represented by the
-- R2 type in RIVSDef. Make it an instance of RIVS.
--
-- Since minus and neg have default implementations using each other, you only
-- need to provide one of them in an instance. Of course it is also harmless to
-- provide both.

-- Example application: {(1,1), (2,0)} is a basis of ℝ², but not orthonormal.
-- Gram-Schmidt can turn it into an orthonormal basis:
--
--     gramschmidt [MkR2 1 1, MkR2 2 0]
--
-- gives: [MkR2 0.0 1.0,MkR2 1.0 0.0], i.e., {(0,1), (1,0)}

instance RIVS R2 where

