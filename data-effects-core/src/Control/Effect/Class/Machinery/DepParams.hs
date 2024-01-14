{-# LANGUAGE AllowAmbiguousTypes #-}
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
-}
module Control.Effect.Class.Machinery.DepParams where

import Control.Effect.Class (
    EffectDataHandler,
    EffectDataToClass,
    EffectsVia,
    Embed,
    Instruction,
    LiftIns,
    SendIns,
    SendSig,
    Signature,
    Tag,
    TagH,
    ViaTag,
 )
import Data.Functor.Identity (Identity)
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

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

{-
- Regarding the Kinds of Dependent Parameters

    Ideally, the kinds should be @DepParams (EffectClassIdentifierOf e)@ instead of @k@. However, it
    seems that kind inference isn't working well, so the kind annotation is intentionally weakened.
-}

-- | Obtain the effect class from the effect class identifier and tuple of dependent parameters.
type family
    EffectClassOf
        (eci :: EffectClassIdentifier)
        (dps :: k) ::
        EffectClass

-- | Obtain the identifier of the effect class.
type family EffectClassIdentifierOfC (e :: EffectClass) :: EffectClassIdentifier

-- | Obtain the dependent parameters portion of the effect class.
type family DepParamsOfC (e :: EffectClass) :: k

{- |
Obtain the /instruction class/ data type from the effect class identifier and tuple of dependent
parameters.
-}
type family
    InsClassOf
        (eci :: EffectClassIdentifier)
        (dps :: k) ::
        Instruction

{- |
Obtain the /signature class/ data type from the effect class identifier, tuple of independent
parameters, and tuple of dependent parameters.
-}
type family
    SigClassOf
        (eci :: EffectClassIdentifier)
        (dps :: k) ::
        Signature

-- | Obtain the identifier of the instruction class.
type family EffectClassIdentifierOf (e :: Instruction) :: EffectClassIdentifier

-- | Obtain the dependent parameters portion of the instruction class.
type family DepParamsOf (e :: Instruction) :: k

-- | Obtain the identifier of the signature class.
type family EffectClassIdentifierOfH (e :: Signature) :: EffectClassIdentifier

-- | Obtain the dependent parameters portion of the instruction class.
type family DepParamsOfH (e :: Signature) :: k

type instance EffectClassIdentifierOfH (LiftIns e) = EffectClassIdentifierOf e
type instance DepParamsOfH (LiftIns e) = DepParamsOf e

-- | Effect class identifier for tagged effect classes.
data I'Tag (eci :: EffectClassIdentifier) tag

-- | Effect class identifier for tagged effect classes.
type (#-) = I'Tag

type instance DepParams (I'Tag eci tag) = DepParams eci
type instance InsClassOf (I'Tag eci tag) dps = Tag (InsClassOf eci dps) tag
type instance SigClassOf (I'Tag eci tag) dps = TagH (SigClassOf eci dps) tag
type instance EffectClassIdentifierOf (Tag e tag) = I'Tag (EffectClassIdentifierOf e) tag
type instance EffectClassIdentifierOfH (TagH e tag) = I'Tag (EffectClassIdentifierOfH e) tag
type instance DepParamsOf (Tag e tag) = DepParamsOf e
type instance DepParamsOfH (TagH e tag) = DepParamsOfH e

data I'Embed f
type instance DepParams (I'Embed f) = ()
type instance InsClassOf (I'Embed f) '() = Embed f
type instance EffectClassIdentifierOf (Embed f) = I'Embed f
type instance DepParamsOf (Embed f) = '()

{- |
Obtain the dependent parameters uniquely associated with the effect class identifier within the
carrier @f@.

If the carrier @f@ does not handle the effect class corresponding to the effect class identifier
@eci@, @'Nothing@ is returned.
-}
type family QueryDepParamsFor (eci :: EffectClassIdentifier) (f :: Type -> Type) :: Maybe k

type family
    FromJustDepParams
        (mDPS :: Maybe k)
        (eci :: EffectClassIdentifier)
        (f :: Type -> Type)
    where
    FromJustDepParams ('Just dps) _ _ = dps
    FromJustDepParams 'Nothing eci f =
        TypeError
            ( 'Text "The carrier: " ':<>: 'ShowType f
                ':$$: 'Text " does not handle the effect class corresponding to the effect class identifier:"
                ':$$: 'Text "    " ':<>: 'ShowType eci
            )

{- |
Obtain the dependent parameters uniquely associated with the effect class identifier within the
carrier @f@.
-}
type DepParamsFor eci f = FromJustDepParams (QueryDepParamsFor eci f) eci f :: k

type instance QueryDepParamsFor eci (EffectsVia EffectDataHandler f) = QueryDepParamsFor eci f
type instance QueryDepParamsFor eci (ViaTag EffectDataHandler tag f) = QueryDepParamsFor (I'Tag eci tag) f
type instance QueryDepParamsFor eci (EffectDataToClass f) = QueryDepParamsFor eci f

type instance QueryDepParamsFor _ Identity = 'Nothing
type instance QueryDepParamsFor _ IO = 'Nothing

-- | A version of t`Control.Effect.Class.SendIns` that supports functional dependency.
type SendInsDep eci f = SendIns (InsClassOf eci (DepParamsFor eci f :: DepParams eci)) f

-- | The operator version of `SendInsDep`.
type eci <:- f = SendInsDep eci f

-- | A version of t`Control.Effect.Class.SendSig` that supports functional dependency.
type SendSigDep eci f = SendSig (SigClassOf eci (DepParamsFor eci f :: DepParams eci)) f

-- | The operator version of `SendSigDep`.
type eci <<:- f = SendSigDep eci f
