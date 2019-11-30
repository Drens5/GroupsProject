module Cyclic.C2 where

-- | Implementation of cyclic group on two elements.

import Group

data C2 = Zero | One
    deriving (Eq, Show)

instance Semigroup C2
  where
    Zero <> a = a
    a <> Zero = a
    One <> One = Zero


instance Monoid C2
  where
    mempty = Zero

instance Group C2
  where
    invert = id