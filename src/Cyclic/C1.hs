module Cyclic.C1 where

-- | Implementation of cyclic group on one element

import Group

data C1 = ZeroC1
    deriving (Eq, Show)

instance Semigroup C1
  where
    _ <> _ = ZeroC1

instance Monoid C1
  where
    mempty = ZeroC1

instance Group C1
  where
    elements = [ZeroC1]
    invert _ = mempty