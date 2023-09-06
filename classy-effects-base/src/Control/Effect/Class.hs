{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class where

import Control.Applicative (Alternative)
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Comp.Multi.Derive (makeHFunctor)
import Data.Kind (Type)

-- | A kind of /signature/ (a datatype of higher-order effect).
type Signature = (Type -> Type) -> (Type -> Type)

-- | A kind of /instruction/ (a datatype of first-order effect).
type Instruction = Type -> Type

-- | A natural transformation.
type f ~> g = forall x. f x -> g x

{- | Lift an /instruction/ to a /signature/.

     Come from [heft-lang\/POPL2023\/haskell\/src\/Elab.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Elab.hs#L9-L10).
-}

-- The code before modification is MIT licensed; (c) 2023 Casper Bach Poulsen and Cas van der Rest.
newtype LiftIns (ins :: Instruction) (f :: Type -> Type) a = LiftIns {unliftIns :: ins a}
    deriving stock (Functor, Foldable, Traversable)

makeHFunctor ''LiftIns

class SendIns (ins :: Instruction) f where
    sendIns :: ins a -> f a

class SendSig (sig :: Signature) f where
    sendSig :: sig f a -> f a

instance SendIns ins f => SendSig (LiftIns ins) f where
    sendSig = sendIns . unliftIns
    {-# INLINE sendSig #-}

newtype EffectsVia handlerSystem (f :: Type -> Type) a = EffectsVia {runEffectsVia :: f a}
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadFix
        , MonadIO
        , MonadFail
        )

newtype ViaTag handlerSystem tag (f :: Type -> Type) a = ViaTag {runViaTag :: f a}
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadFix
        , MonadIO
        , MonadFail
        )

data EffectDataHandler

instance SendIns ins f => SendIns ins (EffectsVia EffectDataHandler f) where
    sendIns = EffectsVia . sendIns

instance (SendSig sig f, HFunctor sig) => SendSig sig (EffectsVia EffectDataHandler f) where
    sendSig = EffectsVia . sendSig . hfmap runEffectsVia

tag ::
    forall tag handlerSystem f a.
    EffectsVia handlerSystem (ViaTag handlerSystem tag f) a ->
    EffectsVia handlerSystem f a
tag = coerce
{-# INLINE tag #-}

newtype Tag (ins :: Instruction) tag a = Tag {getTag :: ins a}
    deriving stock (Functor, Foldable, Traversable)

type (#) = Tag

pattern T :: forall tag ins a. ins a -> Tag ins tag a
pattern T e = Tag e
{-# INLINE T #-}

newtype TagH (sig :: Signature) tag f a = TagH {getTagH :: sig f a}
    deriving stock (Functor, Foldable, Traversable)
    deriving newtype (HFunctor)

type (##) = TagH

pattern TH :: forall tag sig f a. sig f a -> TagH sig tag f a
pattern TH e = TagH e

instance
    SendIns (ins # tag) (EffectsVia EffectDataHandler f) =>
    SendIns ins (ViaTag EffectDataHandler tag f)
    where
    sendIns = ViaTag . runEffectsVia @EffectDataHandler . sendIns . T @tag
    {-# INLINE sendIns #-}

instance
    (SendSig (sig ## tag) (EffectsVia EffectDataHandler f), HFunctor sig) =>
    SendSig sig (ViaTag EffectDataHandler tag f)
    where
    sendSig =
        ViaTag
            . runEffectsVia @EffectDataHandler
            . sendSig
            . TH @tag
            . hfmap coerce
    {-# INLINE sendSig #-}

class Nop (f :: Type -> Type)
instance Nop f

data NopI (a :: Type)
    deriving stock (Functor, Foldable, Traversable)

type NopS = LiftIns NopI
