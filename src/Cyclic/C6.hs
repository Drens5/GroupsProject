module Cyclic.C6 where

import Group
import Cyclic.Cyclic (project)

data C6 = ZeroC6 | OneC6 | TwoC6 | ThreeC6 | FourC6 | FiveC6
    deriving (Eq, Show)

-- | Map C6 onto Z/6Z.
toZmod6Z :: C6 -> Int
toZmod6Z ZeroC6 = 0
toZmod6Z OneC6 = 1
toZmod6Z TwoC6 = 2
toZmod6Z ThreeC6 = 3
toZmod6Z FourC6 = 4
toZmod6Z FiveC6 = 5

-- | Map from Z/6Z onto C6.
fromZmod6Z :: Int -> C6
fromZmod6Z 0 = ZeroC6
fromZmod6Z 1 = OneC6
fromZmod6Z 2 = TwoC6
fromZmod6Z 3 = ThreeC6
fromZmod6Z 4 = FourC6
fromZmod6Z 5 = FiveC6

instance Semigroup C6
  where
    a <> b = fromZmod6Z $ project 6 $ toZmod6Z a + toZmod6Z b

instance Monoid C6
  where
    mempty = ZeroC6

instance Group C6
  where
    elements = [ZeroC6, OneC6, TwoC6, ThreeC6, FourC6, FiveC6]
    invert a = fromZmod6Z $ project 6 $ -(toZmod6Z a)