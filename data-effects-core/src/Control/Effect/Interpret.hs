-- SPDX-License-Identifier: MPL-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Copyright   :  (c) 2025 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect.Interpret where

import Control.Effect (
    Eff (..),
    Free (liftFree, runFree),
    hoist,
    sendUnion,
    type (~>),
    type (~~>),
 )
import Data.Effect (Emb, getEmb)
import Data.Effect.HFunctor (hfmap)
import Data.Effect.OpenUnion (
    Has,
    In,
    KnownOrder,
    Membership,
    Union,
    Weaken,
    extract,
    identityMembership,
    keyMembership,
    labelMembership,
    nil,
    project,
    weakens,
    (!+),
    (:>),
 )
import Data.Effect.Tag (unTag)
import Data.Functor.Identity (Identity, runIdentity)

runEff :: (Free c ff, c f) => Eff ff '[Emb f] a -> f a
runEff = runFree (getEmb . extract) . unEff
{-# INLINE runEff #-}

runPure :: (Free c ff, c Identity) => Eff ff '[] a -> a
runPure = runIdentity . runFree nil . unEff
{-# INLINE runPure #-}

interpret
    :: forall e es ff a c
     . (Free c ff, KnownOrder e)
    => (e ~~> Eff ff es)
    -> Eff ff (e ': es) a
    -> Eff ff es a
interpret i = interpretAll $ i !+ sendUnion
{-# INLINE interpret #-}

reinterpret
    :: forall e es es' ff a c
     . (Weaken es es', Free c ff, KnownOrder e)
    => (e ~~> Eff ff es')
    -> Eff ff (e ': es) a
    -> Eff ff es' a
reinterpret i = interpretAll $ i !+ sendUnion . weakens
{-# INLINE reinterpret #-}

interpose
    :: forall e es ff a c
     . (Free c ff, e :> es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interpose = interposeFor labelMembership
{-# INLINE interpose #-}

interposeOn
    :: forall key e es ff a c
     . (Free c ff, Has key e es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interposeOn f = interposeFor (keyMembership @key) (f . unTag)
{-# INLINE interposeOn #-}

interposeIn
    :: forall e es ff a c
     . (Free c ff, e `In` es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interposeIn = interposeFor identityMembership
{-# INLINE interposeIn #-}

interposeFor
    :: forall e es ff a c
     . (Free c ff, KnownOrder e)
    => Membership e es
    -> (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interposeFor i f =
    interpretAll \u ->
        case project i u of
            Just e -> f e
            Nothing -> sendUnion u
{-# INLINE interposeFor #-}

preinterpose
    :: forall e es ff a c
     . (Free c ff, e :> es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
preinterpose = preinterposeFor labelMembership
{-# INLINE preinterpose #-}

preinterposeOn
    :: forall key e es ff a c
     . (Free c ff, Has key e es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
preinterposeOn f = preinterposeFor (keyMembership @key) (f . unTag)
{-# INLINE preinterposeOn #-}

preinterposeIn
    :: forall e es ff a c
     . (Free c ff, e `In` es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
preinterposeIn = preinterposeFor identityMembership
{-# INLINE preinterposeIn #-}

preinterposeFor
    :: forall e es ff a c
     . (Free c ff, KnownOrder e)
    => Membership e es
    -> (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
preinterposeFor i f = loop
  where
    loop :: Eff ff es ~> Eff ff es
    loop (Eff a) = Eff $ (`runFree` a) \u ->
        hoist (hfmap loop) $ case project i u of
            Just e -> unEff $ f e
            Nothing -> liftFree u
{-# INLINE preinterposeFor #-}

interpretAll
    :: forall es es' ff a c
     . (Free c ff)
    => (Union es ~~> Eff ff es')
    -> Eff ff es a
    -> Eff ff es' a
interpretAll i = loop
  where
    loop :: Eff ff es ~> Eff ff es'
    loop = Eff . runFree (unEff . i . hfmap loop) . unEff
{-# INLINE interpretAll #-}

iterEff
    :: forall es f ff a c
     . (Free c ff, c f)
    => (Union es ~~> f)
    -> Eff ff es a
    -> f a
iterEff i = loop
  where
    loop :: Eff ff es ~> f
    loop = runFree (i . hfmap loop) . unEff
{-# INLINE iterEff #-}
