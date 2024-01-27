{-# LANGUAGE AllowAmbiguousTypes #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.Provider.Implicit where

data ImplicitProvider c i e (f :: Type -> Type) (a :: Type) where
    WithImplicit :: i -> (forall g. (c g, e g) => (forall x. f x -> g x) -> g a) -> ImplicitProvider c i e f a

makeEffectH [''ImplicitProvider]

type MonadImplicitProvider = ImplicitProvider Monad
type ApplicativeImplicitProvider = ImplicitProvider Applicative

(.!) :: forall c e i f a. ImplicitProvider c i e <<: f => i -> (forall g. (c g, e g) => g a) -> f a
i .! m = withImplicit @i @c @e i \_ -> m
{-# INLINE (.!) #-}

(..!) :: forall c e i f a. ImplicitProvider c i e <<: f => i -> (forall g. (c g, e g) => (forall x. f x -> g x) -> g a) -> f a
i ..! f = withImplicit @i @c @e i f
{-# INLINE (..!) #-}
