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
module Data.Effect.Provider where

import Data.Effect.Key (type (##>))
import Data.Functor.Identity (Identity, runIdentity)

data Provider' ctx i hdls b (f :: Type -> Type) (a :: Type) where
    Provide
        :: i
        -> (forall s. hdls s b -> (forall x. f x -> b x) -> b a)
        -> Provider' ctx i hdls b f (ctx a)
makeEffectH [''Provider']

data ProviderKey ctx i hdls

type Provider ctx i hdls b = ProviderKey ctx i hdls ##> Provider' ctx i hdls b
type Provider_ i hdls b = ProviderKey Identity i hdls ##> Provider' Identity i hdls b

infix 2 .!

(.!)
    :: forall i hdls f a b
     . ( SendHOEBy (ProviderKey Identity i hdls) (Provider' Identity i hdls b) f
       , Functor f
       )
    => i
    -> (forall s. hdls s b -> (f ~> b) -> b a)
    -> f a
i .! f = runIdentity <$> provide'' @(ProviderKey Identity i hdls) i f
{-# INLINE (.!) #-}

infix 2 ..!

(..!)
    :: forall ctx i hdls f a b
     . (SendHOEBy (ProviderKey ctx i hdls) (Provider' ctx i hdls b) f)
    => i
    -> (forall s. hdls s b -> (f ~> b) -> b a)
    -> f (ctx a)
i ..! f = provide'' @(ProviderKey ctx i hdls) i f
{-# INLINE (..!) #-}
