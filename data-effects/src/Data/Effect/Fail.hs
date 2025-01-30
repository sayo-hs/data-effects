{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Fail (
    module Data.Effect.Fail,
    Fail (..),
)
where

import Data.Effect (Fail (Fail))

makeEffectF' (def & noGenerateLabel & noGenerateOrderInstance) ''Fail
