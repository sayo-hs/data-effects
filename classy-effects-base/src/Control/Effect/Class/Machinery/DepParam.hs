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
-}
module Control.Effect.Class.Machinery.DepParam where

import Control.Effect.Class (Instruction, LiftIns, SendIns, SendSig, Signature)
import Data.Kind (Constraint, Type)

-- | Kind of the effect class.
type EffectClass = (Type -> Type) -> Constraint

{- |
Kind of the effect class identifier type tag.
Just an alias for the `Type` kind.

For instance, for the effect class

@
class E (a :: Type) (b :: Type) (f :: Type -> Type) | f a -> b where ...
@

its effect class identifier type tag would be like

@
data I'E (a :: Type)
@
-}
type EffectClassIdentifier = Type

{- |
Data-kind of the tuple of dependent parameters for the effect class.

For instance, for the effect class:

@
class E (g :: Int -> Type) (a :: String) (f :: Type -> Type) | f -> g a where ...
@

It would correspond to:

@
data I'E
type instance DepParams I'E = (Int -> Type, String)
@
-}
type family DepParams (eci :: EffectClassIdentifier) :: Type

-- | Obtain the effect class from the effect class identifier and tuple of dependent parameters.
type family
    EffectClassOf
        (eci :: EffectClassIdentifier)
        (dps :: DepParams eci) ::
        EffectClass

{- |
Obtain the /instruction class/ data type from the effect class identifier and tuple of dependent
parameters.
-}
type family
    InsClassOf
        (eci :: EffectClassIdentifier)
        (dps :: DepParams eci) ::
        Instruction

{- |
Obtain the /signature class/ data type from the effect class identifier, tuple of independent
parameters, and tuple of dependent parameters.
-}
type family
    SigClassOf
        (eci :: EffectClassIdentifier)
        (dps :: DepParams eci) ::
        Signature

-- | Obtain the identifier of the instruction class.
type family EffectClassIdentifierOf (e :: Instruction) :: EffectClassIdentifier

-- | Obtain the dependent parameters portion of the instruction class.

{-
Ideally, this should be @DepParams (EffectClassIdentifierOf e)@ instead of @k@. However, it seems
that kind inference on the handler side isn't working well, so the kind annotation is intentionally
weakened here.
-}
type family DepParamsOf (e :: Instruction) :: k

-- | Obtain the identifier of the signature class.
type family EffectClassIdentifierOfH (e :: Signature) :: EffectClassIdentifier

-- | Obtain the dependent parameters portion of the instruction class.

-- Regarding the kind annotation, it is as mentioned in @DepParamsOf@.
type family DepParamsOfH (e :: Signature) :: k

type instance DepParamsOfH (LiftIns e) = DepParamsOf e

{- |
Obtain the dependent parameters uniquely associated with the effect class identifier within the
carrier @f@.
-}
type family DepParamsFor (eci :: EffectClassIdentifier) (f :: Type -> Type) :: k

-- | A version of t`Control.Effect.Class.SendIns` that supports functional dependency.
class
    SendIns (InsClassOf eci (DepParamsFor eci f)) f =>
    SendInsDep eci f

-- | A version of t`Control.Effect.Class.SendSig` that supports functional dependency.
class
    SendSig (SigClassOf eci (DepParamsFor eci f)) f =>
    SendSigDep eci f
