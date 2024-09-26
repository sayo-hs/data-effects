{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides the t`KVStore` effect, comes
from [@Polysemy.KVStore@](https://hackage.haskell.org/package/polysemy-kvstore-0.1.3.0/docs/Polysemy-KVStore.html)
in the @polysemy-kvstore@ package.
-}
module Data.Effect.KVStore where

import Data.Effect.Except (Throw, throw)
import Data.Maybe (isJust)

data KVStore k v a where
    LookupKV :: k -> KVStore k v (Maybe v)
    UpdateKV :: k -> Maybe v -> KVStore k v ()

makeEffectF [''KVStore]

lookupOrThrowKV :: (KVStore k v <: m, Throw e <: m, Monad m) => (k -> e) -> k -> m v
lookupOrThrowKV err k =
    lookupKV k >>= maybe (throw $ err k) pure

existsKV :: forall v k f. (KVStore k v <: f, Functor f) => k -> f Bool
existsKV = fmap isJust . lookupKV @k @v
{-# INLINE existsKV #-}

writeKV :: KVStore k v <: f => k -> v -> f ()
writeKV k v = updateKV k (Just v)
{-# INLINE writeKV #-}

deleteKV :: forall v k f. KVStore k v <: f => k -> f ()
deleteKV k = updateKV @k @v k Nothing
{-# INLINE deleteKV #-}

modifyKV :: (KVStore k v <: m, Monad m) => v -> (v -> v) -> k -> m ()
modifyKV vDefault f k = do
    v <- lookupKV k
    updateKV k (Just $ maybe vDefault f v)
