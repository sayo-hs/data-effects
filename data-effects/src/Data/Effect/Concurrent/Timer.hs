{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.Concurrent.Timer where

import Control.Monad (when)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Time (DiffTime)

data Timer a where
    Clock :: Timer DiffTime
    Sleep :: DiffTime -> Timer ()
makeEffectF [''Timer]

withElapsedTime :: (Timer <: m, Monad m) => (m DiffTime -> m a) -> m a
withElapsedTime f = do
    start <- clock
    f $ clock <&> (`subtract` start)

measureTime :: (Timer <: m, Monad m) => m a -> m (DiffTime, a)
measureTime m = withElapsedTime \elapsedTime -> do
    r <- m
    elapsedTime <&> (,r)

sleepUntil :: (Timer <: m, Monad m) => DiffTime -> m (Maybe DiffTime)
sleepUntil t = do
    now <- clock
    when (t > now) do
        sleep $ t - now
    pure if t < now then Just (now - t) else Nothing

runCyclic :: (Timer <: m, Monad m) => m DiffTime -> m () -> m a
runCyclic deltaTime a = do
    t0 <- clock
    flip fix t0 \next t -> do
        t' <- (t +) <$> deltaTime
        a
        delay <- sleepUntil t'
        next $ maybe t' (t' +) delay

runPeriodic :: (Timer <: m, Monad m) => DiffTime -> m () -> m a
runPeriodic interval = runCyclic (pure interval)
{-# INLINE runPeriodic #-}
