{-# LANGUAGE AllowAmbiguousTypes #-}

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
module Control.Effect.Key where

import Control.Applicative (Alternative)
import Control.Effect (SendIns (sendIns), SendSig (sendSig))
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Data.Coerce (coerce)
import Data.Effect (InsClass, SigClass)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Kind (Type)

class SendInsBy key f (ins :: InsClass) | key f -> ins where
    sendInsBy :: ins a -> f a

class SendSigBy key f (sig :: SigClass) | key f -> sig where
    sendSigBy :: sig f a -> f a

-- | A wrapper data type to represent sending an effect to the carrier @f@ with the specified key.
newtype ByKey key (f :: Type -> Type) a = ByKey {runByKey :: f a}
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

-- | Send all effects within the scope, keyed, to carrier @f@.
key :: forall key f a. ByKey key f a -> f a
key = runByKey
{-# INLINE key #-}

instance SendInsBy key f ins => SendIns ins (ByKey key f) where
    sendIns = ByKey . sendInsBy @key
    {-# INLINE sendIns #-}

instance (SendSigBy key f sig, HFunctor sig) => SendSig sig (ByKey key f) where
    sendSig = ByKey . sendSigBy @key . hfmap coerce
    {-# INLINE sendSig #-}
