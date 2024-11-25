{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   : (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that realize non-deterministic computations.
-}
module Data.Effect.NonDet where

-- | An effect that eliminates a branch by causing the current branch context of a non-deterministic computation to fail.
data Empty :: Effect where
    -- | Eliminates a branch by causing the current branch context of a non-deterministic computation to fail.
    Empty :: Empty f a

makeEffectF ''Empty

-- | An effect that splits the computation into two branches.
data Choose :: Effect where
    -- | Splits the computation into two branches.
    -- As a result of executing @choose@, the world branches into one where `False` is returned and one where `True` is returned.
    Choose :: Choose f Bool

makeEffectF ''Choose

{- |
An effect that executes two branches as scopes.
A higher-order version of the t`Choose` effect.
-}
data ChooseH :: Effect where
    -- | Executes the given two scopes as branches.
    -- Even if one fails due to the `empty` operation, the whole does not fail as long as the other does not fail.
    ChooseH :: f a -> f a -> ChooseH f a

makeEffectH ''ChooseH
