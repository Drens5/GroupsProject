module Cyclic.C2 where

-- | Implementation of cyclic group on two elements.

import Group

data C2 = ZeroC2 | OneC2
    deriving (Eq, Show)

instance Semigroup C2
  where
    ZeroC2 <> a = a
    a <> ZeroC2 = a
    OneC2 <> OneC2 = ZeroC2


instance Monoid C2
  where
    mempty = ZeroC2

instance Group C2
  where
    invert = id