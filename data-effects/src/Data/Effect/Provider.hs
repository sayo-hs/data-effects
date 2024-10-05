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

import Data.Functor.Identity (Identity, runIdentity)

data Provider ctx i hdls (f :: Type -> Type) (a :: Type) where
    Provide :: i -> (hdls f -> f a) -> Provider ctx i hdls f (ctx a)
makeEffectH_ [''Provider]

type Provider_ = Provider Identity

infixl 2 .!

(.!)
    :: forall i hdls f a
     . (Provider_ i hdls <<: f, Functor f)
    => i
    -> (hdls f -> f a)
    -> f a
i .! f = runIdentity <$> provide i f
{-# INLINE (.!) #-}

infixl 2 ..!

(..!)
    :: forall ctx i hdls f a
     . (Provider ctx i hdls <<: f)
    => i
    -> (hdls f -> f a)
    -> f (ctx a)
i ..! f = provide i f
{-# INLINE (..!) #-}
