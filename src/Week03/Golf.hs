module Week03.Golf
  ( skips,
    localMaxima,
    histogram,
  )
where

import Data.List (group, sort, tails)
import qualified Data.Map as Map

-- | First, iterate over all indices of initial list, then, for each index `i`
-- filter initial list zip with index `j` by mod j i, and taking filtered
-- elements to create new list
skips :: [a] -> [[a]]
-- map/filter version:
-- skips x = map (\i -> map fst . filter (\(_, j) -> mod j i == 0) $ zip x [1..]) [1..length x]
-- list comprehension version:
skips x = [[a | (a, j) <- zip x [1 ..], mod j i == 0] | i <- [1 .. length x]]

-- | Create sliding window by applying tails, then taking first 3 elements
-- of every tail, pattern matching those that have exactyl 3 elements, and
-- filtering those that represent local maxima into new list using
-- list comprehension
localMaxima :: [Integer] -> [Integer]
localMaxima x = [b | [a, b, c] <- map (take 3) . tails $ x, b > a && b > c]

-- | Create a map `nums` of digit -> number of occurances, choose height as a
-- maximum number from the map, and populate lines with symbols
histogram :: [Integer] -> String
histogram x =
  unlines . reverse $
    "==========\n0123456789" :
      [ [ if n < i then ' ' else '*' | j <- [0 .. 9], let Just n = Map.lookup j nums
        ]
        | i <- [1 .. maximum . Map.elems $ nums]
      ]
  where
    nums =
      Map.fromList $
        ([0 .. 9] `zip` replicate 10 0)
          ++ (map (\a -> (head a, length a)) . group . sort $ x)
