{-# LANGUAGE UndecidableInstances #-}

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
module Control.Effect.Class.Provider where

import Control.Effect.Class (
    EffectDataHandler,
    EffectsVia (EffectsVia),
    SendSig,
    runEffectsVia,
    sendSig,
    type (~>),
 )
import Control.Effect.Class.Machinery.DepParam (DepParams, DepParamsFor, SendSigDep, SigClassOf)
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap, makeHFunctor)
import Data.Effect.Class.TH (makeSignature)
import Data.Tuple (Solo (Solo))

class Provider c e i g (f :: Type -> Type) where
    provide :: i -> (forall h. (c h, e h) => (f ~> h) -> h a) -> f (g a)

makeSignature ''Provider

instance HFunctor (ProviderS c e i g) where
    hfmap phi (Provide i f) = Provide i \l -> f $ l . phi

instance
    SendSig (ProviderS c e i g) f =>
    Provider c e i g (EffectsVia EffectDataHandler f)
    where
    {-# INLINE provide #-}
    provide i f =
        EffectsVia
            . sendSig
            . hfmap (runEffectsVia @EffectDataHandler)
            $ Provide @c @e i f

type MonadProvider = Provider Monad
type ApplicativeProvider = Provider Applicative

mprovide ::
    MonadProvider e i g f =>
    i ->
    (forall h. (Monad h, e h) => (f ~> h) -> h a) ->
    f (g a)
mprovide = provide
{-# INLINE mprovide #-}

aprovide ::
    ApplicativeProvider e i g f =>
    i ->
    (forall h. (Applicative h, e h) => (f ~> h) -> h a) ->
    f (g a)
aprovide = provide
{-# INLINE aprovide #-}

class CatchDep e (f :: Type -> Type) | f -> e where
    catchDep :: f a -> (e -> f a) -> f a

data I'CatchDep
type instance DepParams I'CatchDep = Solo Type

makeSignature ''CatchDep
makeHFunctor ''CatchDepS

type instance SigClassOf I'CatchDep ('Solo e) = CatchDepS e

instance
    (SendSigDep I'CatchDep f, 'Solo e ~ DepParamsFor I'CatchDep f) =>
    CatchDep e (EffectsVia EffectDataHandler f)
    where
    catchDep x1 x2 =
        EffectsVia
            . sendSig
            . hfmap runEffectsVia
            $ CatchDep x1 x2
