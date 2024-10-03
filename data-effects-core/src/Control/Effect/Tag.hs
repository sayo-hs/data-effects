{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Tag where

import Control.Applicative (Alternative)
import Control.Effect (SendFOE (sendFOE), SendHOE (sendHOE))
import Control.Effect.Key (SendFOEBy, SendHOEBy, sendFOEBy, sendHOEBy)
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

instance (SendFOE (ins # tag) f) => SendFOE ins (ViaTag tag f) where
    sendFOE = ViaTag . sendFOE . T @tag
    {-# INLINE sendFOE #-}

instance (SendHOE (sig ## tag) f, HFunctor sig) => SendHOE sig (ViaTag tag f) where
    sendHOE = ViaTag . sendHOE . TH @tag . hfmap coerce
    {-# INLINE sendHOE #-}

instance (SendFOEBy key (ins # tag) f) => SendFOEBy key ins (ViaTag tag f) where
    sendFOEBy = ViaTag . sendFOEBy @key . T @tag
    {-# INLINE sendFOEBy #-}

instance (SendHOEBy key (sig ## tag) f, HFunctor sig) => SendHOEBy key sig (ViaTag tag f) where
    sendHOEBy = ViaTag . sendHOEBy @key . TH @tag . hfmap coerce
    {-# INLINE sendHOEBy #-}
