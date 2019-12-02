module Subgroup
    ( generateSubgroup
    , generateNormalSubgroup
    , allConjugates
    ) where

import Group
import Data.List

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
generateSubgroupHelper old new = generateSubgroupHelper new $ nub $
    (addedInvs ++) $ concatMap (laction addedInvs) addedInvs -- Only laction
    -- is sufficient, for if ab occurs as a result of laction xs a, then ba
    -- occurs in laction xs b.
  where
    addedInvs = new `union` map invert new

-- | Conjugates an element a by every element in its group.
allConjugates :: (Group m, Eq m) => m -> [m]
allConjugates a = nub $ map (`conjugate` a) (elements :: Group m => [m])

-- | Normal subgroup generated buy subset of group.
-- The smallest normal subgroup containing all the elements in the subset.
generateNormalSubgroup :: (Group m, Eq m) => [m] -> [m]
generateNormalSubgroup = makeNormal . generateSubgroup

-- | Extend a subgroup to the smallest normal subgroup containing the given
-- subgroup.
-- Need some bigger non-abelian groups to test this.
makeNormal :: (Group m, Eq m) => [m] -> [m]
makeNormal = makeNormalHelper []

makeNormalHelper :: (Group m, Eq m) => [m] -> [m] -> [m]
makeNormalHelper old new
  | old == new = new
makeNormalHelper old new = makeNormalHelper new $ nub $ concatMap allConjugates
    new
-- Since mempty is in a subgroup we need not explicitly add new to the result
-- of concatMap.