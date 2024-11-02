{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects for controlling time-related operations.
-}
module Data.Effect.Concurrent.Timer where

import Control.Monad (when)
import Data.Effect.Coroutine (Yield, yield)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Time (DiffTime)

-- | An effect for time-related operations.
data Timer a where
    -- | Retrieves the current relative time from an arbitrary fixed reference point.
    --   The reference point does not change within the context of that scope.
    Clock :: Timer DiffTime
    -- | Temporarily suspends computation for the specified duration.
    Sleep :: DiffTime -> Timer ()

makeEffectF [''Timer]

{- |
Creates a scope where elapsed time can be obtained.
An action to retrieve the elapsed time, re-zeroed at the start of the scope, is passed to the scope.
-}
withElapsedTime
    :: (Timer <: m, Monad m)
    => (m DiffTime -> m a)
    -- ^ A scope where the elapsed time can be obtained.
    -- An action to retrieve the elapsed time is passed as an argument.
    -> m a
withElapsedTime f = do
    start <- clock
    f $ clock <&> (`subtract` start)

-- | Returns the time taken for a computation along with the result as a pair.
measureTime :: (Timer <: m, Monad m) => m a -> m (DiffTime, a)
measureTime m = withElapsedTime \elapsedTime -> do
    r <- m
    elapsedTime <&> (,r)

{- |
Temporarily suspends computation until the relative time from the fixed reference point in the current scope's context, as given by the argument.
If the specified resume time has already passed, returns the elapsed time (positive value) in `Just`.
-}
sleepUntil :: (Timer <: m, Monad m) => DiffTime -> m (Maybe DiffTime)
sleepUntil t = do
    now <- clock
    when (t > now) do
        sleep $ t - now
    pure if t < now then Just (now - t) else Nothing

{- |
Repeats a computation indefinitely. Controls so that each loop occurs at a specific time interval.
If the computation time exceeds and the requested interval cannot be realized, the excess delay occurs, which accumulates and is not canceled.
-}
runCyclic
    :: (Timer <: m, Monad m)
    => m DiffTime
    -- ^ An action called at the start of each loop to determine the time interval until the next loop.
    --   For example, @pure 1@ would control the loop to have a 1-second interval.
    -> m ()
    -- ^ The computation to repeat.
    -> m a
runCyclic deltaTime a = do
    t0 <- clock
    flip fix t0 \next t -> do
        t' <- (t +) <$> deltaTime
        a
        delay <- sleepUntil t'
        next $ maybe t' (t' +) delay

{- |
Controls to repeat a specified computation at fixed time intervals. A specialized version of `runCyclic`.
If the computation time exceeds and the requested interval cannot be realized, the excess delay occurs, which accumulates and is not canceled.
-}
runPeriodic
    :: (Timer <: m, Monad m)
    => DiffTime
    -- ^ Loop interval
    -> m ()
    -- ^ The computation to repeat.
    -> m a
runPeriodic interval = runCyclic (pure interval)
{-# INLINE runPeriodic #-}

{- |
Calls `yield` of a coroutine at fixed intervals.
If the computation time exceeds and the requested interval cannot be realized, the excess delay occurs, which accumulates and is not canceled.
-}
periodicTimer :: forall m a. (Timer <: m, Yield () () <: m, Monad m) => DiffTime -> m a
periodicTimer interval = runPeriodic interval $ yield ()
{-# INLINE periodicTimer #-}

{- |
Calls `yield` of a coroutine at specific intervals.
Controls so that the time returned by `yield` becomes the time interval until the next loop.
If the computation time exceeds and the requested interval cannot be realized, the excess delay occurs, which accumulates and is not canceled.
-}
cyclicTimer :: forall m a. (Timer <: m, Yield () DiffTime <: m, Monad m) => m a
cyclicTimer = runCyclic (yield ()) (pure ())
{-# INLINE cyclicTimer #-}

-- | An effect that realizes control of wait times such that the specified time becomes the interval until the next @wait@ when @wait@ is executed repeatedly.
data CyclicTimer a where
    -- | Controls the wait time so that when @wait@ is executed repeatedly, the specified time becomes the interval until the next @wait@.
    Wait :: DiffTime -> CyclicTimer ()

makeEffectF [''CyclicTimer]
