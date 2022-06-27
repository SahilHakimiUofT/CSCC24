module RIVSDef where

-- A “real inner-product vector space” is a vector space that has an inner
-- product (e.g., dot product for ℝ²), using real numbers (let's say Double) for
-- scalars.  It supports the following operations:

class RIVS a where
    -- zero vector
    zero :: a
    -- adding two vectors
    plus :: a -> a -> a
    -- additive inverse, plus v (neg v) = zero
    neg :: a -> a
    -- subtracting two vectors
    minus :: a -> a -> a
    -- multiplying by scalar
    scale :: Double -> a -> a
    -- inner product
    dot :: a -> a -> Double

    -- Default implementations so an instance can omit one of {minus, neg}
    minus u v = plus u (neg v)
    neg v = minus zero v


-- The benefit of having RIVS is that algorithms for inner-product spaces can be
-- coded up polymorphically:

-- Project a vector on to a “plane”.
-- Assume that ws is an orthonormal list of vectors.
-- project v ws = project v on to the “plane” spanned by ws.
project :: RIVS a => a -> [a] -> a
project v ws = foldl plus zero [scale (dot v w) w | w <- ws]

-- The Gram-Schmidt algorithm converts a linearly independent list of vectors to
-- an orthonormal list of vectors of the same span. In other words, compute an
-- orthonormal basis.
gramschmidt :: RIVS a => [a] -> [a]
gramschmidt [] = []
gramschmidt (v:vs) =
  let bs = gramschmidt vs
      w = minus v (project v bs)
      wlen :: Double
      wlen = sqrt (dot w w)
  in scale (1/wlen) w : bs


-- ℝ² is a well known real inner-product vector space, and is represented by the
-- R2 type below. In RIVS.hs, it is your turn to make it an instance of RIVS.

data R2 = MkR2 Double Double
    deriving (Eq, Show)
