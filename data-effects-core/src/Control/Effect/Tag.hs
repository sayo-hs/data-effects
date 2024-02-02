{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Tag where

import Control.Applicative (Alternative)
import Control.Effect (SendIns (sendIns), SendSig (sendSig))
import Control.Effect.Key (SendInsBy, SendSigBy, sendInsBy, sendSigBy)
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Data.Coerce (coerce)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.Tag (pattern T, pattern TH, type (#), type (##))
import Data.Kind (Type)

-- | A wrapper data type to represent sending an effect to the carrier @f@ with the specified tag.
newtype ViaTag tag (f :: Type -> Type) a = ViaTag {runViaTag :: f a}
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadFix
        , MonadIO
        , MonadFail
        , MonadReader r
        , MonadWriter w
        , MonadState s
        , MonadRWS r w s
        , MonadError e
        )

-- | Send all effects within the scope, tagged, to carrier @f@.
tag :: forall tag f a. ViaTag tag f a -> f a
tag = runViaTag
{-# INLINE tag #-}

instance SendIns (ins # tag) f => SendIns ins (ViaTag tag f) where
    sendIns = ViaTag . sendIns . T @tag
    {-# INLINE sendIns #-}

instance (SendSig (sig ## tag) f, HFunctor sig) => SendSig sig (ViaTag tag f) where
    sendSig = ViaTag . sendSig . TH @tag . hfmap coerce
    {-# INLINE sendSig #-}

instance SendInsBy key f (ins # tag) => SendInsBy key (ViaTag tag f) ins where
    sendInsBy = ViaTag . sendInsBy @key . T @tag
    {-# INLINE sendInsBy #-}

instance (SendSigBy key f (sig ## tag), HFunctor sig) => SendSigBy key (ViaTag tag f) sig where
    sendSigBy = ViaTag . sendSigBy @key . TH @tag . hfmap coerce
    {-# INLINE sendSigBy #-}
