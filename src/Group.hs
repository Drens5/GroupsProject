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


-- | Subgroup generated by subset of group.
-- The smallest subgroup containing all the elements in the subset.
generateSubgroup :: (Group m, Eq m) => [m] -> [m]
generateSubgroup subset = generateSubgroupHelper [] $ nub subset

-- | Generates subgroup from given subset by repeatedly adding inverses and
-- multiplying each element with eachother (on both sides) until the result of
-- doing so is the same as the previous iteration.
--
-- It's best to call this with new not having duplicates as to avoid an extra
-- run if it's done in one step.
generateSubgroupHelper :: (Group m, Eq m) => [m] -> [m] -> [m]
generateSubgroupHelper old new
  | old == new = new
generateSubgroupHelper _ [] = []
generateSubgroupHelper old new = generateSubgroupHelper new $ nub $
    (addedInvs ++) $ concatMap (laction addedInvs) addedInvs -- Only laction
    -- is sufficient, for if ab occurs as a result of laction xs a, then ba
    -- occurs in laction xs b.
  where
    addedInvs = new `union` map invert new

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