{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects for holding mutable state values in the context.
-}
module Data.Effect.State where

-- | An effect for holding mutable state values in the context.
data State s :: Effect where
    -- | Retrieves the current state value from the context.
    Get :: State s f s
    -- | Overwrites the state value in the context.
    Put :: s -> State s f ()

makeEffectF ''State

-- | Retrieves the current state value from the context and returns the value transformed based on the given function.
gets :: (State s <! f, Functor f) => (s -> a) -> f a
gets f = f <$> get

-- | Modifies the current state value in the context based on the given function.
modify :: (State s <! m, Monad m) => (s -> s) -> m ()
modify f = put . f =<< get
