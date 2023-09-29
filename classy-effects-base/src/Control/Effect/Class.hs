{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
               (c) 2023 Casper Bach Poulsen and Cas van der Rest
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides the essential definitions required for CEPs compliance, forming the foundation
of classy-effects.
Please refer to [CEPs](https://github.com/sayo-hs/classy-effects/blob/master/CEPs/README.md) for details.
-}
module Control.Effect.Class where

import Control.Applicative (Alternative)
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Comp.Multi.Derive (makeHFunctor)
import Data.Kind (Type)

-- | A kind of /signature class/ (a datatype of higher-order effect).
type Signature = (Type -> Type) -> (Type -> Type)

-- | A kind of /instruction class/ (a datatype of first-order effect).
type Instruction = Type -> Type

-- | A natural transformation.
type f ~> g = forall x. f x -> g x

{- | Lift an /instruction class/ to a /signature class/.

     Come from [heft-lang\/POPL2023\/haskell\/src\/Elab.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Elab.hs#L9-L10).
-}

{-  The code before modification is licensed under the MIT License as
    shown in [1]. The modified code, in its entirety, is licensed under
    MPL 2.0. When redistributing, please ensure that you do not remove
    the MIT License text as indicated in [1].

    [1] Copyright (c) 2023 Casper Bach Poulsen and Cas van der Rest

        Permission is hereby granted, free of charge, to any person obtaining
        a copy of this software and associated documentation files (the
        "Software"), to deal in the Software without restriction, including
        without limitation the rights to use, copy, modify, merge, publish,
        distribute, sublicense, and/or sell copies of the Software, and to
        permit persons to whom the Software is furnished to do so, subject to
        the following conditions:

        The above copyright notice and this permission notice shall be
        included in all copies or substantial portions of the Software.

        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
        EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
        MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
        NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
        LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
        OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
        WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
newtype LiftIns (ins :: Instruction) (f :: Type -> Type) a = LiftIns {unliftIns :: ins a}
    deriving stock (Functor, Foldable, Traversable)

makeHFunctor ''LiftIns

-- | A type class that represents the ability to send an /instruction/ @ins@ to carrier @f@.
class SendIns (ins :: Instruction) f where
    -- | Send an /instruction/ @ins@ to carrier @f@.
    sendIns :: ins a -> f a

-- | The operator version of `SendIns`.
type (<:) = SendIns

-- | A type class that represents the ability to send a /signature/ @sig@ to carrier @f@.
class SendSig (sig :: Signature) f where
    -- | Send a /signature/ @sig@ to carrier @f@.
    sendSig :: sig f a -> f a

-- | The operator version of `SendSig`.
type (<<:) = SendIns

instance SendIns ins f => SendSig (LiftIns ins) f where
    sendSig = sendIns . unliftIns
    {-# INLINE sendSig #-}

-- | A wrapper data type to integrate a backend handler system into the classy-effects framework.
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

-- | A wrapper data type to represent sending an effect to the carrier @f@ with the specified tag.
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

{- |
A backend identifier type tag to represent the mechanism of sending effects to a carrier using
`SendIns`/`SendSig` based on CEP-02.
-}
data EffectDataHandler

instance SendIns ins f => SendIns ins (EffectsVia EffectDataHandler f) where
    sendIns = EffectsVia . sendIns

instance (SendSig sig f, HFunctor sig) => SendSig sig (EffectsVia EffectDataHandler f) where
    sendSig = EffectsVia . sendSig . hfmap runEffectsVia

-- | A type class representing the carrier @f@ capable of sending tagged effects.
class Taggable f where
    -- | A wrapper type to send to carrier @f@ with the specified tag.
    type Tagged f tag :: Type -> Type

    -- | Send all effects within the scope, tagged, to carrier @f@.
    unTagged :: Tagged f tag a -> f a

instance Taggable (EffectsVia handlerSystem f) where
    type Tagged (EffectsVia handlerSystem f) tag = EffectsVia handlerSystem (ViaTag handlerSystem tag f)
    unTagged = coerce
    {-# INLINE unTagged #-}

-- | Send all effects within the scope, tagged, to carrier @f@.
tag :: forall tag f a. Taggable f => Tagged f tag a -> f a
tag = unTagged @_ @tag
{-# INLINE tag #-}

-- | A wrapper type to send to carrier @f@ with the specified tag.
type f @# tag = Tagged f tag

-- | Tagged /instruction class/.
newtype Tag (ins :: Instruction) tag a = Tag {getTag :: ins a}
    deriving stock (Functor, Foldable, Traversable)

-- | Tagged /instruction class/.
type (#) = Tag

-- | Tagged /instruction class/.
pattern T :: forall tag ins a. ins a -> Tag ins tag a
pattern T e = Tag e
{-# INLINE T #-}

-- | Tagged /signature class/.
newtype TagH (sig :: Signature) tag f a = TagH {getTagH :: sig f a}
    deriving stock (Functor, Foldable, Traversable)
    deriving newtype (HFunctor)

-- | Tagged /signature class/.
type (##) = TagH

-- | Tagged /signature class/.
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

-- | An /effect class/ with no effects.
class Nop (f :: Type -> Type)

instance Nop f

-- | An /instruction class/ with no effects.
data NopI (a :: Type)
    deriving stock (Functor, Foldable, Traversable)

-- | A /signature class/ with no effects.
type NopS = LiftIns NopI
