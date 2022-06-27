module Merge where

-- There are other questions in other files.

-- Take two lists of integers.  Precondition: Each is already sorted
-- (non-decreasing).  Merge them in non-decreasing order.  Linear time.
--
-- Example:
-- merge [2, 3, 5] [1, 3, 4, 4, 7, 7] = [1, 2, 3, 3, 4, 4, 5, 7, 7]
--
-- The given template guides thinking about base cases and recursive cases, but
-- you can change it to your liking after you understand.

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x : xt) (y : yt) 
    |x<y = x:y:xl
    |y<=x = y:x:xl
        where xl = merge xt yt
