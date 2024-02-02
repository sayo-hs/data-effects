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

This module provides the `Provider` effect, comes
from [@Effectful.Provider@](https://hackage.haskell.org/package/effectful-core-2.3.0.0/docs/Effectful-Provider.html)
in the @effectful@ package.
-}
module Data.Effect.Provider where

data Provider' c i ctx e (f :: Type -> Type) (a :: Type) where
    Provide ::
        i ->
        (forall g. (c g, e g) => (forall x. f x -> g x) -> g a) ->
        Provider' c i ctx e f (ctx a)

makeKeyedEffect [] [''Provider']

type MonadProvider' = Provider' Monad
type ApplicativeProvider' = Provider' Applicative

type MonadProvider i ctx e = Provider Monad i ctx e
type ApplicativeProvider i ctx e = Provider Applicative i ctx e

mprovide ::
    forall e i ctx f a.
    SendSigBy ProviderKey f (MonadProvider' i ctx e) =>
    i ->
    (forall g. (Monad g, e g) => (forall x. f x -> g x) -> g a) ->
    f (ctx a)
mprovide = provide
{-# INLINE mprovide #-}

aprovide ::
    forall e i ctx f a.
    SendSigBy ProviderKey f (ApplicativeProvider' i ctx e) =>
    i ->
    (forall h. (Applicative h, e h) => (forall x. f x -> h x) -> h a) ->
    f (ctx a)
aprovide = provide
{-# INLINE aprovide #-}
