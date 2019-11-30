{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Group where

-- | Definition of group.
class Monoid m => Group m
  where
    invert :: m -> m

-- | Implementation of direct product of groups.
data DirectProduct a b = DP a b -- Maybe I have to use typefamilies for this after all.

instance (Monoid a, Monoid b) => Monoid (DirectProduct a b)
  where
    (DP a1 b1) <> (DP a2 b2) = DP (a1 <> a2) (b1 <> b2)
    mempty = DP (mempty :: a) (mempty :: b)

instance (Group a, Group b) => Group (DirectProduct a b)
  where
    invert (DP a b) = DP (invert a) (invert b)
