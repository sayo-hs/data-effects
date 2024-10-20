{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.ShiftReset where

import Control.Monad ((>=>))
import Data.Effect.Key (KeyH (KeyH))
import Data.Functor (void)

data Shift' (ans :: Type) n m a where
    Shift
        :: forall ans n m a
         . ((a -> n ans) -> (forall x. m x -> n x) -> n ans)
        -> Shift' ans n m a
makeKeyedEffect [] [''Shift']

callCC
    :: forall a m ans n
     . (SendHOEBy ShiftKey (Shift' ans n) m, Monad m, Monad n)
    => ((a -> n ans) -> m a)
    -> m a
callCC f = shift \k run -> run (f $ k >=> run . exit) >>= k

exit
    :: forall a m ans n
     . (SendHOEBy ShiftKey (Shift' ans n) m, Applicative n)
    => ans
    -> m a
exit ans = shift \_ _ -> pure ans
{-# INLINE exit #-}

getCC
    :: forall m ans n
     . (SendHOEBy ShiftKey (Shift' ans n) m, Monad m, Monad n)
    => m (n ans)
getCC = callCC \exit' -> let a = exit' a in pure a

embed :: forall m ans n. (SendHOEBy ShiftKey (Shift' ans n) m, Monad n) => n ~> m
embed m = shift \k _ -> m >>= k
{-# INLINE embed #-}

data Shift_' n m a where
    Shift_'
        :: (forall (ans :: Type). (a -> n ans) -> (forall x. m x -> n x) -> n ans)
        -> Shift_' n m a
makeKeyedEffect [] [''Shift_']

getCC_ :: forall m n. (SendHOEBy Shift_Key (Shift_' n) m, Functor n) => m (n ())
getCC_ = shift_' \k _ -> let k' = k $ void k' in k'

data Reset m (a :: Type) where
    Reset :: m a -> Reset m a
makeEffectH [''Reset]

data ShiftF ans a where
    ShiftF :: forall ans a. ((a -> ans) -> ans) -> ShiftF ans a
makeEffectF [''ShiftF]

fromShiftF :: ShiftF (n ans) ~> Shift ans n m
fromShiftF (ShiftF f) = KeyH $ Shift \k _ -> f k
{-# INLINE fromShiftF #-}

exitF :: forall ans m a. (ShiftF ans <: m) => ans -> m a
exitF ans = shiftF @ans $ const ans
{-# INLINE exitF #-}

embedF :: forall ans n m. (ShiftF (n ans) <: m, Monad n) => n ~> m
embedF m = shiftF @(n ans) (m >>=)
{-# INLINE embedF #-}
