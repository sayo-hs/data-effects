{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Example where

import Data.Effect.HFunctor (HFunctor)
import Data.Effect.HFunctor.TH (makeHFunctor, makeHFunctor')
import Data.Effect.TH (makeEffect, makeEffectH)
import Data.Kind (Type)
import Data.List.Infinite (Infinite ((:<)))

data Throw e (a :: Type) where
    Throw :: e -> Throw e a

data Catch e f (a :: Type) where
    Catch :: f a -> (e -> f a) -> Catch e f a

makeEffect [''Throw] [''Catch]

data Unlift b f (a :: Type) where
    WithRunInBase :: ((forall x. f x -> b x) -> b a) -> Unlift b f a

makeEffectH [''Unlift]

data Nested (f :: Type -> Type) (a :: Type) where
    Nested :: ([f a -> Int] -> Int) -> Nested f a
makeHFunctor ''Nested

data ManuallyCxt (g :: Type -> Type) h (f :: Type -> Type) (a :: Type) where
    ManuallyCxt :: g (h f a) -> ManuallyCxt g h f a
makeHFunctor' ''ManuallyCxt \(g :< h :< _) -> [t|(Functor $g, HFunctor $h)|]

data NestedTuple (f :: Type -> Type) (a :: Type) where
    NestedTuple :: ((forall x. (f x, f a) -> Int) -> Int) -> NestedTuple f a
makeHFunctor ''NestedTuple

newtype IdentityH f (a :: Type) = IdentityH {unIdentityH :: f a}
makeHFunctor ''IdentityH
