{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module provides the t`Input` effect, comes
from [@Polysemy.Input@](https://hackage.haskell.org/package/polysemy-1.9.1.1/docs/Polysemy-Input.html)
in the @polysemy@ package.

Realizes input of values from the external world.
-}
module Data.Effect.Input where

-- | A general effect representing input of values from the external world.
data Input i :: Effect where
    -- | Retrieve a value input from the external world.
    Input :: Input i f i

makeEffectF ''Input

-- | Returns the value obtained by transforming the input value using the given function.
inputs :: (Input i <! f, Functor f) => (i -> a) -> f a
inputs f = f <$> input
