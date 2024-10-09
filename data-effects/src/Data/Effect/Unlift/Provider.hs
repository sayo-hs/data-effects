{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides the `Provider` effect, like [@Effectful.Provider@](https://hackage.haskell.org/package/effectful-core-2.3.0.0/docs/Effectful-Provider.html)
in the @effectful@ package.
-}
module Data.Effect.Unlift.Provider where

import Data.Effect.Key (type (##>))
import Data.Functor.Identity (Identity, runIdentity)

data Provider' ctx i b (f :: Type -> Type) (a :: Type) where
    Provide
        :: i
        -> ((forall x. f x -> b x) -> b a)
        -> Provider' ctx i b f (ctx a)
makeEffectH [''Provider']

data ProviderKey ctx i

type Provider ctx i b = ProviderKey ctx i ##> Provider' ctx i b
type Provider_ i b = ProviderKey Identity i ##> Provider' Identity i b

infix 2 .!

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

(..!)
    :: forall ctx i f a b
     . (SendHOEBy (ProviderKey ctx i) (Provider' ctx i b) f)
    => i
    -> ((f ~> b) -> b a)
    -> f (ctx a)
i ..! f = provide'' @(ProviderKey ctx i) i f
{-# INLINE (..!) #-}
