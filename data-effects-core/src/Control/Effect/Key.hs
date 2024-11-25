{-# LANGUAGE AllowAmbiguousTypes #-}

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
module Control.Effect.Key where

import Control.Applicative (Alternative)
import Control.Effect (Perform (perform))
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter)
import Data.Coerce (coerce)
import Data.Effect (Effect)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Kind (Type)

class PerformBy key (e :: Effect) f | key f -> e where
    performBy :: e f a -> f a

-- | A wrapper data type to represent performing an effect to the carrier @f@ with the specified key.
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

-- | Perform all effects within the scope, keyed, on the carrier @f@.
key :: forall key f a. ByKey key f a -> f a
key = runByKey
{-# INLINE key #-}

instance (PerformBy key e f, HFunctor e) => Perform e (ByKey key f) where
    perform = ByKey . performBy @key . hfmap coerce
    {-# INLINE perform #-}
