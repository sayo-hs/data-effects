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
import Control.Effect (Perform (perform))
import Control.Effect.Key (PerformBy (performBy))
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
import Data.Effect.Tag (pattern Tag, type (#))
import Data.Kind (Type)

-- | A wrapper data type to represent performing an effect on the carrier @f@ with the specified tag.
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

-- | Perform all effects within the scope, tagged, on the carrier @f@.
tag :: forall tag f a. ViaTag tag f a -> f a
tag = runViaTag
{-# INLINE tag #-}

instance (Perform (e # tag) f, HFunctor e) => Perform e (ViaTag tag f) where
    perform = ViaTag . perform . Tag @tag . hfmap coerce
    {-# INLINE perform #-}

instance (PerformBy key (e # tag) f, HFunctor e) => PerformBy key e (ViaTag tag f) where
    performBy = ViaTag . performBy @key . Tag @tag . hfmap coerce
    {-# INLINE performBy #-}
