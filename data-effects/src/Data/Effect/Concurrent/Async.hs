{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Concurrent.Async where

import Data.Effect.Concurrent.Parallel (Parallel (LiftP2))

data Async' f ans a where
    Fork :: Async' f ans (Either (a -> f ans) (f a))
    Finish :: f ans -> Async' f ans a
    Await :: forall f ans a. f a -> Async' f ans a

makeKeyedEffect [''Async'] []

parallelToAsync :: forall m f ans. (SendFOEBy AsyncKey (Async' f ans) m, Monad m) => Parallel m ~> m
parallelToAsync (LiftP2 f a b) =
    fork >>= \case
        Left send -> finish . send =<< a
        Right recv -> do
            y <- b
            x <- await recv
            pure $ f x y

liftAsync2
    :: forall a b c m ans f
     . (SendFOEBy AsyncKey (Async' f ans) m, Monad m)
    => (a -> b -> c)
    -> m a
    -> m b
    -> m c
liftAsync2 f a b = parallelToAsync $ LiftP2 f a b
{-# INLINE liftAsync2 #-}

liftAsync3
    :: forall a b c d m ans f
     . (SendFOEBy AsyncKey (Async' f ans) m, Monad m)
    => (a -> b -> c -> d)
    -> m a
    -> m b
    -> m c
    -> m d
liftAsync3 f a b c =
    parallelToAsync $ LiftP2 ($) (parallelToAsync $ LiftP2 f a b) c
{-# INLINE liftAsync3 #-}
