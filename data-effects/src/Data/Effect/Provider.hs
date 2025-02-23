{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module provides the `Provider` effect, like [@Effectful.Provider@](https://hackage.haskell.org/package/effectful-core-2.3.0.0/docs/Effectful-Provider.html)
in the @effectful@ package.
-}
module Data.Effect.Provider where

import Control.Effect.Transform (raise, raisePrefix, raisePrefix1)
import Data.Effect.OpenUnion (EachEffect, KnownLength, type (++))
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (Identity), runIdentity)

-- | An effect to introduce a new local scope that provides effect context @b s@.
data Scope t i b :: Effect where
    -- | Introduces a new local scope that provides an effect context @b s@ parameterized by type @i s@ and with results wrapped in @t s@.
    Scope
        :: forall s t i a f b
         . i s
        -> ((forall x. f x -> b s x) -> b s a)
        -> Scope t i b f (t s a)

makeEffectH' (def & noGenerateLabel) ''Scope

-- | A type-level label to uniquely resolve the effect context carrier @b@ from @t@ and @i@.
data ScopeLabel (t :: k -> Type -> Type) (i :: k -> Type)

type instance LabelOf (Scope t i b) = ScopeLabel t i

newtype Const1 f x (a :: Type) = Const1 {getConst1 :: f a}

-- | An effect to introduce a new local scope that provides the scope-parametrized effect @e@.
type Scoped ff t i es r = Scope t i (ScopeC ff t i es r)

newtype ScopeC ff t i fs r s a
    = ScopeC {unScopeC :: Eff ff (EachEffect fs s ++ Scoped ff t i fs r ': r) a}

-- | An effect to introduce a new local scope that provides the effect @e@.
type Scoped_ ff t i e es = Scope t i (Const1 (ScopeC_ ff t i e es))

newtype ScopeC_ ff t i es r a
    = ScopeC_ {unScopeC_ :: Eff ff (es ++ Scoped_ ff t i es r ': r) a}

type Provider ff t i e es = Scoped_ ff (Const1 t) (Const i :: () -> Type) e es

runScoped
    :: forall t i a es r ff c
     . (Free c ff, KnownLength es)
    => ( forall s x
          . i s
         -> Eff ff (EachEffect es s ++ Scoped ff t i es r ': r) x
         -> Eff ff (Scoped ff t i es r ': r) (t s x)
       )
    -> Eff ff (Scoped ff t i es r ': r) a
    -> Eff ff r a
runScoped run = loop
  where
    loop :: Eff ff (Scoped ff t i es r ': r) ~> Eff ff r
    loop = interpret \(Scope (i :: i s) f) ->
        loop $ run i (unScopeC $ f $ ScopeC . raisePrefix1 @es @s . raise @(Scoped ff t i es r))
{-# INLINE runScoped #-}

scoped
    :: forall s t i a es' es r ff c
     . (Scoped ff t i es r :> es', Free c ff)
    => i s
    -> ( Eff ff es' ~> Eff ff (EachEffect es s ++ Scoped ff t i es r ': r)
         -> Eff ff (EachEffect es s ++ Scoped ff t i es r ': r) a
       )
    -> Eff ff es' (t s a)
scoped i f = scope i \pop -> ScopeC $ f $ unScopeC . pop
{-# INLINE scoped #-}

runScoped_
    :: forall t i a es r ff c
     . (Free c ff, KnownLength es)
    => ( forall p x
          . i p
         -> Eff ff (es ++ Scoped_ ff t i es r ': r) x
         -> Eff ff (Scoped_ ff t i es r ': r) (t p x)
       )
    -> Eff ff (Scoped_ ff t i es r ': r) a
    -> Eff ff r a
runScoped_ run = loop
  where
    loop :: Eff ff (Scoped_ ff t i es r ': r) ~> Eff ff r
    loop = interpret \(Scope i f) ->
        loop $ run i (unScopeC_ . getConst1 $ f $ Const1 . ScopeC_ . raisePrefix @es . raise @(Scoped_ ff t i es r))
{-# INLINE runScoped_ #-}

scoped_
    :: forall s t i a es' es r ff c
     . (Scoped_ ff t i es r :> es', Free c ff)
    => i s
    -> ( Eff ff es' ~> Eff ff (es ++ Scoped_ ff t i es r ': r)
         -> Eff ff (es ++ Scoped_ ff t i es r ': r) a
       )
    -> Eff ff es' (t s a)
scoped_ i f = scope i \pop -> Const1 $ ScopeC_ $ f $ unScopeC_ . getConst1 . pop
{-# INLINE scoped_ #-}

runProvider
    :: forall t i a es r ff c
     . (forall f. (c (ff f)) => Functor (ff f), Free c ff, KnownLength es)
    => ( forall x
          . i
         -> Eff ff (es ++ Provider ff t i es r ': r) x
         -> Eff ff (Provider ff t i es r ': r) (t x)
       )
    -> Eff ff (Provider ff t i es r ': r) a
    -> Eff ff r a
runProvider run = runScoped_ \(Const i) a -> Const1 <$> run i a
{-# INLINE runProvider #-}

provide
    :: forall t i a es' es r ff c
     . (Provider ff t i es r :> es', forall f. (c (ff f)) => Functor (ff f), Free c ff)
    => i
    -> ( Eff ff es' ~> Eff ff (es ++ Provider ff t i es r ': r)
         -> Eff ff (es ++ Provider ff t i es r ': r) a
       )
    -> Eff ff es' (t a)
provide i f = getConst1 <$> scoped_ (Const i) f
{-# INLINE provide #-}

runProvider_
    :: forall i a es r ff c
     . (forall f. (c (ff f)) => Functor (ff f), Free c ff, KnownLength es)
    => ( forall x
          . i
         -> Eff ff (es ++ Provider ff Identity i es r ': r) x
         -> Eff ff (Provider ff Identity i es r ': r) x
       )
    -> Eff ff (Provider ff Identity i es r ': r) a
    -> Eff ff r a
runProvider_ run = runProvider \i a -> Identity <$> run i a
{-# INLINE runProvider_ #-}

provide_
    :: forall i a es' es r ff c
     . (Provider ff Identity i es r :> es', forall f. (c (ff f)) => Functor (ff f), Free c ff)
    => i
    -> ( Eff ff es' ~> Eff ff (es ++ Provider ff Identity i es r ': r)
         -> Eff ff (es ++ Provider ff Identity i es r ': r) a
       )
    -> Eff ff es' a
provide_ i f = runIdentity <$> provide i f
{-# INLINE provide_ #-}

runProvider__
    :: forall a es r ff c
     . (forall f. (c (ff f)) => Functor (ff f), Free c ff, KnownLength es)
    => ( forall x
          . Eff ff (es ++ Provider ff Identity () es r ': r) x
         -> Eff ff (Provider ff Identity () es r ': r) x
       )
    -> Eff ff (Provider ff Identity () es r ': r) a
    -> Eff ff r a
runProvider__ run = runProvider_ \() -> run
{-# INLINE runProvider__ #-}

provide__
    :: forall a es' es r ff c
     . (Provider ff Identity () es r :> es', forall f. (c (ff f)) => Functor (ff f), Free c ff)
    => ( Eff ff es' ~> Eff ff (es ++ Provider ff Identity () es r ': r)
         -> Eff ff (es ++ Provider ff Identity () es r ': r) a
       )
    -> Eff ff es' a
provide__ = provide_ ()
{-# INLINE provide__ #-}
