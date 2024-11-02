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
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity, runIdentity)

-- | An effect to introduce a new local scope that provides effect context @b@.
data Provider' ctx i b (f :: Type -> Type) (a :: Type) where
    -- | Introduces a new local scope that provides an effect context @b p@ parameterized by type @i p@ and with results wrapped in @ctx p@.
    Provide
        :: i p
        -> ((forall x. f x -> b p x) -> b p a)
        -> Provider' ctx i b f (ctx p a)

makeEffectH [''Provider']

-- | A type-level key to uniquely resolve the effect context carrier @b@ from @ctx@ and @i@.
data ProviderKey ctx i

-- | An effect to introduce a new local scope that provides effect context @b@.
type Provider ctx i b = ProviderKey ctx i ##> Provider' ctx i b

{- |
An effect to introduce a new local scope that provides effect context @b@.
A version of `Provider` where the result is not wrapped in a specific container.
-}
type Provider_ i b = Provider (Const1 Identity) (Const i :: () -> Type) (Const1 b)

newtype Const1 f x a = Const1 {getConst1 :: f a}

infix 2 .!

{- | A operator to introduce a new local scope that provides effect context @b@.
A version of `..!` where the result is not wrapped in a specific container.
-}
(.!)
    :: forall i f a b
     . ( SendHOEBy
            (ProviderKey (Const1 Identity :: () -> Type -> Type) (Const i :: () -> Type))
            (Provider' (Const1 Identity) (Const i) (Const1 b))
            f
       , Functor f
       )
    => i
    -> ((f ~> b) -> b a)
    -> f a
i .! f =
    runIdentity . getConst1
        <$> provide'' @(ProviderKey (Const1 Identity :: () -> _ -> _) (Const i :: () -> _))
            (Const i)
            \run -> Const1 $ f $ getConst1 . run
{-# INLINE (.!) #-}

infix 2 ..!

-- | A operator to introduce a new local scope that provides effect context @b p@.
(..!)
    :: forall ctx i p f a b
     . (SendHOEBy (ProviderKey ctx i) (Provider' ctx i b) f)
    => i p
    -> ((f ~> b p) -> b p a)
    -> f (ctx p a)
i ..! f = provide'' @(ProviderKey ctx i) i f
{-# INLINE (..!) #-}
