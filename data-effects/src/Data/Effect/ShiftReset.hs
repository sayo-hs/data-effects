{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.ShiftReset where

import Control.Monad ((>=>))
import Data.Functor (void)

data Shift' (r :: Type) b m a where
    Shift :: forall r b m a. ((a -> b r) -> m r) -> Shift' r b m a

makeKeyedEffect [] [''Shift']

callCC
    :: forall r b m a
     . ( SendHOEBy ShiftKey (Shift' r b) m
       , Monad m
       , SendHOEBy ShiftKey (Shift' r b) b
       , Monad b
       )
    => (b ~> m)
    -> ((a -> b r) -> m a)
    -> m a
callCC lift f = shift \k -> f (k >=> exit) >>= lift . k

exit :: (SendHOEBy ShiftKey (Shift' r b) m, Applicative m) => r -> m a
exit r = shift \_ -> pure r
{-# INLINE exit #-}

getCC
    :: forall r b m
     . ( SendHOEBy ShiftKey (Shift' r b) m
       , SendHOEBy ShiftKey (Shift' r b) b
       , Monad m
       , Monad b
       )
    => (b ~> m)
    -> m (m r)
getCC lift = callCC @r @b @m lift \exit' ->
    let a = exit' a'
        a' = lift a
     in pure a'

data Shift_' b m a where
    Shift_' :: (forall (r :: Type). (a -> b r) -> m r) -> Shift_' b m a

makeKeyedEffect [] [''Shift_']

getCC_ :: (SendHOEBy Shift_Key (Shift_' b) m, Monad m, Functor b) => b ~> m -> m (b ())
getCC_ lift = shift_' \k -> let k' = k $ void k' in lift k'

data Reset m (a :: Type) where
    Reset :: m a -> Reset m a

makeEffectH [''Reset]
