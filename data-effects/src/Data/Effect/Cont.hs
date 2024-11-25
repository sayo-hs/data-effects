{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Cont where

data CallCC :: Effect where
    CallCC :: (forall r. (a -> m r) -> m a) -> CallCC m a

makeEffectH_ ''CallCC
