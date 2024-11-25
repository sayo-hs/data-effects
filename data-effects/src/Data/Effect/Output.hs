{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module provides the t`Output` effect, comes
from [@Polysemy.Output@](https://hackage.haskell.org/package/polysemy-1.9.1.1/docs/Polysemy-Output.html)
in the @polysemy@ package.

Realizes output of values to the external world.
-}
module Data.Effect.Output where

-- | A general effect representing output of values to the external world.
data Output o :: Effect where
    -- | Output a value to the external world.
    Output :: o -> Output o f ()

makeEffectF ''Output
