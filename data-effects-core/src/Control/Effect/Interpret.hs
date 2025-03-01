{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect.Interpret where

import Control.Arrow ((>>>))
import Control.Effect (
    Eff (..),
    Free (liftFree),
    retract,
    type (~>),
    type (~~>),
 )
import Data.Effect (Emb (Emb))
import Data.Effect.HandlerVec (
    HandlerVec,
    Has,
    In,
    KnownOrder,
    empty,
    hcfmapVec,
    hfmapElem,
    identityMembership,
    keyMembership,
    labelMembership,
    overrideFor,
    suffix,
    vmapVec,
    (!:),
    (:>),
 )
import Data.Effect.HandlerVec qualified as H
import Data.Effect.HandlerVec qualified as VH
import Data.Effect.HandlerVec.Rec (
    Membership,
    Suffix,
    type (++),
 )
import Data.Effect.Tag (unTag)
import Data.Functor.Identity (Identity, runIdentity)

runEff :: (Free c ff, c f) => Eff ff '[Emb f] a -> f a
runEff (Eff f) = retract $ f $ H.singleton \(Emb a) -> liftFree a
{-# INLINE runEff #-}

runAllEff :: HandlerVec es (Eff ff es) (ff r) -> Eff ff es a -> ff r a
runAllEff hdl (Eff f) = f hdl
{-# INLINE runAllEff #-}

runPure :: (Free c ff, c Identity) => Eff ff '[] a -> a
runPure (Eff f) = runIdentity $ retract $ f empty
{-# INLINE runPure #-}

interpret
    :: forall e es ff a
     . (KnownOrder e)
    => (e ~~> Eff ff es)
    -> Eff ff (e ': es) a
    -> Eff ff es a
interpret i = transEff \v -> runAllEff v . i !: v
{-# INLINE interpret #-}

reinterpret
    :: forall e es es' ff a
     . (Suffix es es', KnownOrder e)
    => (e ~~> Eff ff es')
    -> Eff ff (e ': es) a
    -> Eff ff es' a
reinterpret i = transEff \v -> runAllEff v . i !: suffix v
{-# INLINE reinterpret #-}

interprets
    :: forall es r ff a
     . HandlerVec es (Eff ff r) (Eff ff r)
    -> Eff ff (es ++ r) a
    -> Eff ff r a
interprets i = transEff \v -> vmapVec (runAllEff v) i `VH.concat` v
{-# INLINE interprets #-}

reinterprets
    :: forall es r r' ff a
     . (Suffix r r')
    => HandlerVec es (Eff ff r') (Eff ff r')
    -> Eff ff (es ++ r) a
    -> Eff ff r' a
reinterprets i = transEff \v -> vmapVec (runAllEff v) i `VH.concat` suffix @r v
{-# INLINE reinterprets #-}

interpose
    :: forall e es ff a
     . (e :> es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interpose = interposeFor labelMembership
{-# INLINE interpose #-}

interposeOn
    :: forall key e es ff a
     . (Has key e es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interposeOn f = interposeFor (keyMembership @key) (f . unTag)
{-# INLINE interposeOn #-}

interposeIn
    :: forall e es ff a
     . (e `In` es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interposeIn = interposeFor identityMembership
{-# INLINE interposeIn #-}

interposeFor
    :: forall e es ff a
     . (KnownOrder e)
    => Membership e es
    -> (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
interposeFor i f = transEff \v -> overrideFor i (runAllEff v . f) v
{-# INLINE interposeFor #-}

transEff
    :: forall a es es' ff
     . ( forall r
          . HandlerVec es' (Eff ff es') (ff r)
         -> HandlerVec es (Eff ff es') (ff r)
       )
    -> Eff ff es a
    -> Eff ff es' a
transEff f = loop
  where
    loop :: Eff ff es ~> Eff ff es'
    loop (Eff g) = Eff $ g . hcfmapVec loop . f
{-# INLINE transEff #-}

preinterpose
    :: forall e es ff a
     . (e :> es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
preinterpose = preinterposeFor labelMembership
{-# INLINE preinterpose #-}

preinterposeOn
    :: forall key e es ff a
     . (Has key e es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
preinterposeOn f = preinterposeFor (keyMembership @key) (f . unTag)
{-# INLINE preinterposeOn #-}

preinterposeIn
    :: forall e es ff a
     . (e `In` es)
    => (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
preinterposeIn = preinterposeFor identityMembership
{-# INLINE preinterposeIn #-}

preinterposeFor
    :: forall e es ff a
     . (KnownOrder e)
    => Membership e es
    -> (e ~~> Eff ff es)
    -> Eff ff es a
    -> Eff ff es a
preinterposeFor i f = loop
  where
    loop :: Eff ff es ~> Eff ff es
    loop (Eff g) = Eff $ hcfmapVec loop >>> \v -> g $ overrideFor i (runAllEff v . f) v
{-# INLINE preinterposeFor #-}

iterEff
    :: forall e f ff a c
     . (KnownOrder e, c f, Free c ff)
    => (e ~~> f)
    -> Eff ff '[e] a
    -> f a
iterEff i = loop
  where
    loop :: Eff ff '[e] ~> f
    loop = retract . runAllEff (H.singleton $ liftFree . i . hfmapElem loop)
{-# INLINE iterEff #-}

-- | /O(n)/ where /n/ = @length es@
iterAllEff
    :: forall es f ff a c
     . (Free c ff, c f)
    => HandlerVec es (Eff ff es) f
    -> Eff ff es a
    -> f a
iterAllEff hdl = retract . runAllEff (vmapVec liftFree hdl)
{-# INLINE iterAllEff #-}
