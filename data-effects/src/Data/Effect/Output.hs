{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides the t`Output` effect, comes
from [@Polysemy.Output@](https://hackage.haskell.org/package/polysemy-1.9.1.1/docs/Polysemy-Output.html)
in the @polysemy@ package.
-}
module Data.Effect.Output where

data Output o a where
    Output :: o -> Output o ()

makeEffectF [''Output]
