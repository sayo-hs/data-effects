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
    KnownOrder,
    LabelResolver,
    Membership,
    Union,
    extract,
    membership,
    nil,
    project,
    (!+),
    (:>),
 )
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

interpose
    :: forall e es ff a c
     . (Free c ff, e :> es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interpose = interposeFor $ membership @LabelResolver
{-# INLINE interpose #-}

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
preinterpose = preinterposeFor $ membership @LabelResolver
{-# INLINE preinterpose #-}

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
