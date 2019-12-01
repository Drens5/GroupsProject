module Cyclic.Cyclic where

-- | Helper module on the implementation of cyclic groups.

import Group

-- | Map from Z onto Z/nZ always yielding a representant from {0, 1, ..., n - 1}.
project :: Int -> Int -> Int
project n a = a `mod` n