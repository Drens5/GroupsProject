module Cyclic.C1 where

-- | Implementation of cyclic group on one element

import Group

data C1 = Zero
    deriving (Eq, Show)

instance Semigroup C1
  where
    _ <> _ = Zero

instance Monoid C1
  where
    mempty = Zero

instance Group C1
  where
    invert _ = mempty