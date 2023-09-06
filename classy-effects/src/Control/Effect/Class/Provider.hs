{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Provider where

import Control.Effect.Class (
    EffectDataHandler,
    EffectsVia (EffectsVia),
    SendSig,
    runEffectsVia,
    sendSig,
    type (~>),
 )
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Data.Effect.Class.TH (
    makeSignature,
 )

class Provider c i g (f :: Type -> Type) where
    provide :: i -> (forall h. c h => (f ~> h) -> h a) -> f (g a)

makeSignature ''Provider

instance HFunctor (ProviderS c i g) where
    hfmap phi (Provide i f) = Provide i \l -> f $ l . phi

instance
    SendSig (ProviderS c i g) f =>
    Provider c i g (EffectsVia EffectDataHandler f)
    where
    {-# INLINE provide #-}
    provide i f =
        EffectsVia
            . sendSig
            . hfmap (runEffectsVia @EffectDataHandler)
            $ Provide @_ @c i f
