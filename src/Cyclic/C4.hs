module Cyclic.C4 where

import Group
import Cyclic.Cyclic (project)

data C4 = ZeroC4 | OneC4 | TwoC4 | ThreeC4
    deriving (Eq, Show)

-- | Map C4 onto Z/4Z.
toZmod4Z :: C4 -> Int
toZmod4Z ZeroC4 = 0
toZmod4Z OneC4 = 1
toZmod4Z TwoC4 = 2
toZmod4Z ThreeC4 = 3

-- | Map from Z/4Z onto C4.
fromZmod4Z :: Int -> C4
fromZmod4Z 0 = ZeroC4
fromZmod4Z 1 = OneC4
fromZmod4Z 2 = TwoC4
fromZmod4Z 3 = ThreeC4

instance Semigroup C4
  where
    a <> b = fromZmod4Z $ project 4 $ toZmod4Z a + toZmod4Z b

instance Monoid C4
  where
    mempty = ZeroC4

instance Group C4
  where
    elements = [ZeroC4, OneC4, TwoC4, ThreeC4]
    invert a = fromZmod4Z $ project 4 $ -(toZmod4Z a)