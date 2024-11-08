{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.SubJump where

import Data.Function (fix)

data SubJump' ref a where
    SubFork :: SubJump' ref (Either (ref a) a)
    Jump :: ref a -> a -> SubJump' ref b

makeKeyedEffect [''SubJump'] []

sub :: (SendFOEBy SubJumpKey (SubJump' ref) m, Monad m) => (ref a -> m b) -> (a -> m b) -> m b
sub p q = subFork >>= either p q
{-# INLINE sub #-}

callCC :: (SendFOEBy SubJumpKey (SubJump' ref) m, Monad m) => ((a -> m b) -> m a) -> m a
callCC f = sub (f . jump) pure
{-# INLINE callCC #-}

getCC :: (SendFOEBy SubJumpKey (SubJump' ref) m, Monad m) => m (m a)
getCC = callCC $ pure . fix
{-# INLINE getCC #-}
