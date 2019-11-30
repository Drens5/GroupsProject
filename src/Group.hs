{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Group where

-- | Definition of group.
class Monoid m => Group m
  where
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
    invert (DP a b) = DP (invert a) (invert b)
