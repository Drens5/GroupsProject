{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Group where

import Data.List

-- | Definition of group.
class Monoid m => Group m
  where
    elements :: [m]
    invert :: m -> m
    pow :: m -> Int -> m
    pow a 1 = a
    pow a (-1) = invert a
    pow a 0 = mempty
    pow a n
      | n < 0 = a <> pow a (n + 1)
      | n > 0 = a <> pow a (n - 1)


-- | Implementation of direct product of groups.
data DirectProduct a b = DP a b
    deriving (Eq, Show)

instance (Group a, Group b) => Semigroup (DirectProduct a b)
  where
    (DP a1 b1) <> (DP a2 b2) = DP (a1 <> a2) (b1 <> b2)

instance (Group a, Group b) => Monoid (DirectProduct a b)
  where
    mempty = DP (mempty :: (Monoid a) => a) (mempty :: (Monoid b) => b)

instance (Group a, Group b) => Group (DirectProduct a b)
  where
    elements = [DP a b | (a :: a) <- elements :: [a],
        (b :: b) <- elements :: [b]]
    invert (DP a b) = DP (invert a) (invert b)

-- | Gives the order of a groupelement.
-- The integer to which power you have to raise the element to get mempty.
order :: (Group m, Eq m) => m -> Int
order a = (1 +) $ length $ takeWhile (/= mempty) [a `pow` n | n <- [1..]]

-- | Left action of a group element on the subset.
-- If the subset is a subgroup this gives the left coset.
laction :: Group m => [m] -> m -> [m]
laction as a = map (a <>) as

-- | Right action of a group element on the subset.
-- If the subset is a subgroup this gives the right coset.
raction :: Group m => [m] -> m -> [m]
raction as a = map (<> a) as

-- | Conjugates an element a by g, that is a -> gag^(-1).
conjugate :: Group m => m -> m -> m
conjugate g a = g <> a <> invert g