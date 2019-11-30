module Cyclic.C3 where

import Group
import Cyclic.Cyclic (project)

data C3 = ZeroC3 | OneC3 | TwoC3
    deriving (Eq, Show)

-- | Map C3 onto Z/3Z.
toZmod3Z :: C3 -> Int
toZmod3Z ZeroC3 = 0
toZmod3Z OneC3 = 1
toZmod3Z TwoC3 = 2

-- | Map from Z/3Z onto C3.
fromZmod3Z :: Int -> C3
fromZmod3Z 0 = ZeroC3
fromZmod3Z 1 = OneC3
fromZmod3Z 2 = TwoC3

instance Semigroup C3
  where
    a <> b = fromZmod3Z $ project 3 $ toZmod3Z a + toZmod3Z b

instance Monoid C3
  where
    mempty = ZeroC3

instance Group C3
  where
    invert a = fromZmod3Z $ project 3 $ -(toZmod3Z a)