{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.SubJump where

import Data.Function (fix)

data SubJump' ref :: Effect where
    SubFork :: SubJump' ref f (Either (ref a) a)
    Jump :: ref a -> a -> SubJump' ref f b

makeKeyedEffectF ''SubJump'

sub :: (PerformBy SubJumpKey (SubJump' ref) m, Monad m) => (ref a -> m b) -> (a -> m b) -> m b
sub p q = subFork >>= either p q
{-# INLINE sub #-}

callCC :: (PerformBy SubJumpKey (SubJump' ref) m, Monad m) => ((a -> m b) -> m a) -> m a
callCC f = sub (f . jump) pure
{-# INLINE callCC #-}

getCC :: (PerformBy SubJumpKey (SubJump' ref) m, Monad m) => m (m a)
getCC = callCC $ pure . fix
{-# INLINE getCC #-}
