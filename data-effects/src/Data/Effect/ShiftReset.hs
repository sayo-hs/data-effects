{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.ShiftReset where

import Control.Monad ((>=>))
import Data.Functor (void)

data Shift' (r :: Type) b m a where
    Shift :: forall r b m a. ((a -> b r) -> (forall x. m x -> b x) -> b r) -> Shift' r b m a

makeKeyedEffect [] [''Shift']

callCC
    :: forall r b m a
     . (SendHOEBy ShiftKey (Shift' r b) m, Monad m, Monad b)
    => ((a -> b r) -> m a)
    -> m a
callCC f = shift \k run -> run (f $ k >=> run . exit) >>= k

exit :: (SendHOEBy ShiftKey (Shift' r b) m, Applicative b) => r -> m a
exit r = shift \_ _ -> pure r
{-# INLINE exit #-}

getCC
    :: forall r b m
     . (SendHOEBy ShiftKey (Shift' r b) m, Monad m, Monad b)
    => m (b r)
getCC = callCC \exit' -> let a = exit' a in pure a

data Shift_' b m a where
    Shift_' :: (forall (r :: Type). (a -> b r) -> (forall x. m x -> b x) -> b r) -> Shift_' b m a

makeKeyedEffect [] [''Shift_']

getCC_ :: (SendHOEBy Shift_Key (Shift_' b) m, Functor b) => m (b ())
getCC_ = shift_' \k _ -> let k' = k $ void k' in k'

data Reset m (a :: Type) where
    Reset :: m a -> Reset m a

makeEffectH [''Reset]
