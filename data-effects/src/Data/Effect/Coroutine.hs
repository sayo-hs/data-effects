{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   : (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
              (c) 2023-2024 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module provides effects for the coroutine, comes
from [@Control.Monad.Freer.Coroutine@](https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Control-Monad-Freer-Coroutine.html)
in the @freer-simple@ package (The continuation part @(b -> c)@ has been removed. If necessary, please manually compose the t`Data.Functor.Coyoneda.Coyoneda`) .
-}
module Data.Effect.Coroutine where

import Control.Monad ((>=>))

{- |
An effect for coroutines.
Realizes an operation that transfers control to the caller of the computation with coroutines along with a value of type @a@,
and receives a value of type @b@ from the caller.
-}
data Yield a b (c :: Type) where
    -- | Transfers control to the caller of the computation with coroutines along with a value of type @a@, and receives a value of type @b@ from the caller.
    Yield :: a -> Yield a b b

makeEffectF [''Yield]

-- | A version of `yield` where the value returned from the caller of the computation with coroutines is unit.
yield_ :: (Yield a () <: f) => a -> f ()
yield_ = yield
{-# INLINE yield_ #-}

{- |
The execution result when handling a computation that includes the t`Yield` effect. A computation that may include suspension.
If the computation does not include `yield`, the completed result of the computation is obtained as `Done`.
If the computation includes `yield`, execution is suspended at that point, and the value of type @a@ thrown by `yield` and the continuation of the computation from the point of `yield` are obtained as `Continue`.
By re-executing this continuation, you can resume the computation.
-}
data Status f a b r
    = -- | The computation has completely finished
      Done r
    | -- | The computation has been suspended by `yield`
      Continue a (b -> f (Status f a b r))
    deriving (Functor)

-- | Extends the computation result by appending the specified continuation.
continueStatus
    :: (Monad m)
    => (x -> m (Status m a b r))
    -- ^ Additional continuation
    -> Status m a b x
    -- ^ Computation status to extend
    -> m (Status m a b r)
continueStatus kk = \case
    Done x -> kk x
    Continue a k -> pure . Continue a $ k >=> continueStatus kk

-- | Repeats the computation until the final result is obtained by continuing the computation using the specified handler each time it suspends.
loopStatus
    :: (Monad m)
    => (a -> m b)
    -- ^ Handler to resume computation from a suspended state.
    -> Status m a b r
    -- ^ A computation that may include suspension.
    -> m r
loopStatus f = \case
    Done r -> pure r
    Continue a k -> f a >>= k >>= loopStatus f
