module Cyclic.C5 where

import Group
import Cyclic.Cyclic (project)

data C5 = ZeroC5 | OneC5 | TwoC5 | ThreeC5 | FourC5
    deriving (Eq, Show)

-- | Map C5 onto Z/5Z.
toZmod5Z :: C5 -> Int
toZmod5Z ZeroC5 = 0
toZmod5Z OneC5 = 1
toZmod5Z TwoC5 = 2
toZmod5Z ThreeC5 = 3
toZmod5Z FourC5 = 4

-- | Map from Z/5Z onto C5.
fromZmod5Z :: Int -> C5
fromZmod5Z 0 = ZeroC5
fromZmod5Z 1 = OneC5
fromZmod5Z 2 = TwoC5
fromZmod5Z 3 = ThreeC5
fromZmod5Z 4 = FourC5

instance Semigroup C5
  where
    a <> b = fromZmod5Z $ project 5 $ toZmod5Z a + toZmod5Z b

instance Monoid C5
  where
    mempty = ZeroC5

instance Group C5
  where
    elements = [ZeroC5, OneC5, TwoC5, ThreeC5, FourC5]
    invert a = fromZmod5Z $ project 5 $ -(toZmod5Z a)