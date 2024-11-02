{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module provides the `Provider` effect, like [@Effectful.Provider@](https://hackage.haskell.org/package/effectful-core-2.3.0.0/docs/Effectful-Provider.html)
in the @effectful@ package.
-}
module Data.Effect.Provider where

import Data.Effect.Key (type (##>))
import Data.Functor.Identity (Identity, runIdentity)

-- | An effect to introduce a new local scope that provides effect context @b@.
data Provider' ctx i b (f :: Type -> Type) (a :: Type) where
    -- | Introduces a new local scope that provides an effect context @b@ parameterized by type @i@ and with results wrapped in @ctx@.
    Provide
        :: i
        -> ((forall x. f x -> b x) -> b a)
        -> Provider' ctx i b f (ctx a)

makeEffectH [''Provider']

-- | A type-level key to uniquely resolve the effect context carrier @b@ from @ctx@ and @i@.
data ProviderKey ctx i

-- | An effect to introduce a new local scope that provides effect context @b@.
type Provider ctx i b = ProviderKey ctx i ##> Provider' ctx i b

{- |
An effect to introduce a new local scope that provides effect context @b@.
A version of `Provider` where the result is not wrapped in a specific container.
-}
type Provider_ i b = ProviderKey Identity i ##> Provider' Identity i b

infix 2 .!

{- | A operator to introduce a new local scope that provides effect context @b@.
A version of `..!` where the result is not wrapped in a specific container.
-}
(.!)
    :: forall i f a b
     . ( SendHOEBy (ProviderKey Identity i) (Provider' Identity i b) f
       , Functor f
       )
    => i
    -> ((f ~> b) -> b a)
    -> f a
i .! f = runIdentity <$> provide'' @(ProviderKey Identity i) i f
{-# INLINE (.!) #-}

infix 2 ..!

-- | A operator to introduce a new local scope that provides effect context @b@.
(..!)
    :: forall ctx i f a b
     . (SendHOEBy (ProviderKey ctx i) (Provider' ctx i b) f)
    => i
    -> ((f ~> b) -> b a)
    -> f (ctx a)
i ..! f = provide'' @(ProviderKey ctx i) i f
{-# INLINE (..!) #-}
