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
module Data.Effect.Except where

data Throw e (a :: Type) where
    Throw :: e -> Throw e a

data Catch e f (a :: Type) where
    Catch :: f a -> (e -> f a) -> Catch e f a

makeEffect [''Throw] [''Catch]

liftEither :: (Throw e <: f, Applicative f) => Either e a -> f a
liftEither = either throw pure
{-# INLINE liftEither #-}

joinEither :: (Throw e <: m, Monad m) => m (Either e a) -> m a
joinEither = (>>= either throw pure)
{-# INLINE joinEither #-}

joinExcept :: Monad m => Either (m a) a -> m a
joinExcept = either id pure
{-# INLINE joinExcept #-}

exc :: Monad m => m (Either (m a) a) -> m a
exc = (>>= either id pure)
{-# INLINE exc #-}
