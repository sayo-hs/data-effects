{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Fail (
    module Data.Effect.Fail,
    Fail (..),
)
where

import Data.Effect (Fail (Fail))

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''Fail
