{-# LANGUAGE DuplicateRecordFields #-}

-- | Implementation of cyclic groups.
-- I feel like there is an abstraction in defining these...
module Cyclic where

import Group
import Data.Monoid

-- | Cyclic group on one element. Rather have a natural number there.
data C1 = C1 Int
    deriving (Eq, Show, Integral)

instance Monoid C1
  where
    _ <> _ = C1 0
    mempty = C1 0

instance Group C1
  where
    invert _ = mempty

-- Cyclic group on two elements.
data C2 = C2 Int
    deriving (Eq, Show, Integral)

instance Monoid C2
  where
    a <> b = C2 $ (a + b) `mod` 2
    mempty = C2 0

instance Group C2
  where
    invert mempty = mempty
    invert (C2 1) = C2 1