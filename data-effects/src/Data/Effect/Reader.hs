{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that can be used to hold environmental values in the context.
Environmental values are immutable and do not change across procedures, but you
can modify the value within a local scope using the `local` operation.
-}
module Data.Effect.Reader where

-- | An effect that holds a value of type @r@ in the context (environment).
data Ask r :: Effect where
    -- | Obtain a value from the environment.
    Ask :: Ask r f r

-- | An effect that locally modifies the value held in the environment.
data Local r :: Effect where
    -- | Locally modifies the value held in the environment.
    Local
        :: (r -> r)
        -- ^ A function that transforms the original value to the modified value.
        -> f a
        -- ^ The local scope where the modification is applied.
        -> Local r f a

makeEffectF ''Ask
makeEffectH ''Local

-- | Obtains a value from the environment and returns it transformed by the given function.
asks :: (Ask r :> es, Functor (Eff ff es), Free c ff) => (r -> a) -> Eff ff es a
asks f = f <$> ask
