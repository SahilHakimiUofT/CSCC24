module Split where

-- There are other questions in other files.

-- Split a list into two, more or less equal length.  Linear time.
--
-- Since lists are sequential, we want to "uninterleave", e.g.,
--
-- split [3,1,2,9] = ([3,2], [1,9])
-- split [3,1,2,9,7] = ([3,2,7], [1,9])
-- split [3] = ([3], [])
--
-- You may like to know: the tuple syntax "(foo, bar)" can be used in pattern
-- matching.
--
-- The given template guides thinking about base cases and recursive cases (and
-- shows "yes you can do that"), but you can change it to your liking after you
-- understand.

split :: [a] -> ([a], [a])
split [] = ([], [])
split (x : []) = ([x],[])
split (x1 : x2 : xt) = (x1:xs,x2:xy)
    where (xs,xy) = split(xt)
