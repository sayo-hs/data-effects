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
    ViaFree (ViaFree, unViaFree),
    retract,
    runAllEff,
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

runEff :: (Free c ff, c f) => Eff (ViaFree ff) '[Emb f] a -> f a
runEff (Eff m) = retract $ unViaFree $ m id $ H.singleton \(Emb a) -> ViaFree $ liftFree a
{-# INLINE runEff #-}

runPure :: (Free c ff, c Identity) => Eff (ViaFree ff) '[] a -> a
runPure (Eff m) = runIdentity $ retract $ unViaFree $ m id empty
{-# INLINE runPure #-}

interpret
    :: forall e es ff a
     . (KnownOrder e)
    => (e ~~> Eff ff es)
    -> Eff ff (e ': es) a
    -> Eff ff es a
interpret i = transEff \kk v -> runAllEff kk v . i !: v
{-# INLINE interpret #-}

reinterpret
    :: forall e es es' ff a
     . (Suffix es es', KnownOrder e)
    => (e ~~> Eff ff es')
    -> Eff ff (e ': es) a
    -> Eff ff es' a
reinterpret i = transEff \kk v -> runAllEff kk v . i !: suffix v
{-# INLINE reinterpret #-}

interprets
    :: forall es r ff a
     . HandlerVec es (Eff ff r) (Eff ff r)
    -> Eff ff (es ++ r) a
    -> Eff ff r a
interprets i = transEff \kk v -> vmapVec (runAllEff kk v) i `VH.concat` v
{-# INLINE interprets #-}

reinterprets
    :: forall es r r' ff a
     . (Suffix r r')
    => HandlerVec es (Eff ff r') (Eff ff r')
    -> Eff ff (es ++ r) a
    -> Eff ff r' a
reinterprets i = transEff \kk v -> vmapVec (runAllEff kk v) i `VH.concat` suffix @r v
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
interposeFor i f = transEff \kk v -> overrideFor i (runAllEff kk v . f) v
{-# INLINE interposeFor #-}

transEff
    :: forall es es' a ff
     . ( forall r es''
          . (Eff ff es' ~> Eff ff es'')
         -> HandlerVec es' (Eff ff es') (ff (Eff ff es'') r)
         -> HandlerVec es (Eff ff es') (ff (Eff ff es'') r)
       )
    -> Eff ff es a
    -> Eff ff es' a
transEff f = loop
  where
    loop :: Eff ff es ~> Eff ff es'
    loop (Eff m) = Eff \kk v -> m (kk . loop) (hcfmapVec loop $ f kk v)
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
    loop (Eff g) = Eff \kk ->
        hcfmapVec loop >>> \v ->
            g kk $ overrideFor i (runAllEff kk v . f) v
{-# INLINE preinterposeFor #-}

iterEff
    :: forall e f ff a c
     . (KnownOrder e, c f, Free c ff)
    => (e ~~> f)
    -> Eff (ViaFree ff) '[e] a
    -> f a
iterEff i = loop
  where
    loop :: Eff (ViaFree ff) '[e] ~> f
    loop = retract . unViaFree . runAllEff id (H.singleton $ ViaFree . liftFree . i . hfmapElem loop)
{-# INLINE iterEff #-}

-- | /O(n)/ where /n/ = @length es@
iterAllEff
    :: forall es f ff a c
     . (Free c ff, c f)
    => HandlerVec es (Eff (ViaFree ff) es) f
    -> Eff (ViaFree ff) es a
    -> f a
iterAllEff hdl = retract . unViaFree . runAllEff id (vmapVec (ViaFree . liftFree) hdl)
{-# INLINE iterAllEff #-}
