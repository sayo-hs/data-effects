{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Concurrent.Async where

import Data.Effect.Concurrent.Parallel (Parallel (LiftP2))
import Data.Void (Void, absurd)

{-

This is inspired by the following ideas:

- By https://github.com/kory33
    - https://x.com/Kory__3/status/1847712676295954673
    - https://x.com/Kory__3/status/1847715858304639349
    - https://x.com/Kory__3/status/1847722148665970791

- Daan Leijen, Structured Asynchrony with Algebraic Effects (2017)

-}

data Async' f a where
    Fork :: forall a f. Async' f (Either (a -> f Void) (Future f a))
    Perform :: forall a f. f a -> Async' f a

data Future f a
    = Future {await :: f a, poll :: f (Maybe a), cancel :: f ()}

makeKeyedEffect [''Async'] []

finish :: (SendFOEBy AsyncKey (Async' f) m, Functor m) => f Void -> m a
finish = fmap absurd . perform
{-# INLINE finish #-}

parallelToAsync
    :: forall m f
     . (SendFOEBy AsyncKey (Async' f) m, Monad m)
    => Parallel m ~> m
parallelToAsync (LiftP2 f a b) =
    fork >>= \case
        Left send -> finish . send =<< a
        Right Future{await} -> do
            y <- b
            x <- perform await
            pure $ f x y

async
    :: forall a m f
     . (SendFOEBy AsyncKey (Async' f) m, Monad m)
    => m a
    -> m (Future f a)
async a =
    fork >>= \case
        Left send -> finish . send =<< a
        Right recv -> pure recv

liftAsync2
    :: forall a b c m f
     . (SendFOEBy AsyncKey (Async' f) m, Monad m)
    => (a -> b -> c)
    -> m a
    -> m b
    -> m c
liftAsync2 f a b = parallelToAsync $ LiftP2 f a b
{-# INLINE liftAsync2 #-}

liftAsync3
    :: forall a b c d m f
     . (SendFOEBy AsyncKey (Async' f) m, Monad m)
    => (a -> b -> c -> d)
    -> m a
    -> m b
    -> m c
    -> m d
liftAsync3 f a b c =
    parallelToAsync $ LiftP2 ($) (parallelToAsync $ LiftP2 f a b) c
{-# INLINE liftAsync3 #-}
