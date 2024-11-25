{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.ShiftReset where

import Control.Monad ((>=>))
import Data.Effect.Key (Keyed (Key))
import Data.Functor (void)

data Shift' (ans :: Type) n :: Effect where
    Shift
        :: forall ans n m a
         . ((a -> n ans) -> (forall x. m x -> n x) -> n ans)
        -> Shift' ans n m a
makeKeyedEffectH ''Shift'

callCC
    :: forall a m ans n
     . (PerformBy ShiftKey (Shift' ans n) m, Monad m, Monad n)
    => ((a -> n ans) -> m a)
    -> m a
callCC f = shift \k run -> run (f $ k >=> run . abort) >>= k

abort
    :: forall a m ans n
     . (PerformBy ShiftKey (Shift' ans n) m, Applicative n)
    => ans
    -> m a
abort ans = shift \_ _ -> pure ans
{-# INLINE abort #-}

getCC
    :: forall m ans n
     . (PerformBy ShiftKey (Shift' ans n) m, Monad m, Monad n)
    => m (n ans)
getCC = callCC \exit' -> let a = exit' a in pure a

embed :: forall m ans n. (PerformBy ShiftKey (Shift' ans n) m, Monad n) => n ~> m
embed m = shift \k _ -> m >>= k
{-# INLINE embed #-}

data Reset m (a :: Type) where
    Reset :: m a -> Reset m a
makeEffectH ''Reset

data Shift_' n m a where
    Shift_'
        :: (forall (ans :: Type). (a -> n ans) -> (forall x. m x -> n x) -> n ans)
        -> Shift_' n m a
{-# DEPRECATED Shift_' "Use Data.Effect.SubJump" #-}

makeKeyedEffectH ''Shift_'

getCC_ :: forall m n. (PerformBy Shift_Key (Shift_' n) m, Functor n) => m (n ())
getCC_ = shift_' \k _ -> let k' = k $ void k' in k'
{-# DEPRECATED Shift_, Shift_Key, shift_', shift_'', shift_''', getCC_ "Use Data.Effect.SubJump" #-}

data ShiftF ans :: Effect where
    ShiftF :: forall ans f a. ((a -> ans) -> ans) -> ShiftF ans f a
{-# DEPRECATED ShiftF "Use Data.Effect.SubJump" #-}
makeEffectF ''ShiftF

fromShiftF :: ShiftF (n ans) f ~> Shift ans n m
fromShiftF (ShiftF f) = Key $ Shift \k _ -> f k
{-# INLINE fromShiftF #-}

exitF :: forall ans m a. (ShiftF ans <! m) => ans -> m a
exitF ans = shiftF @ans $ const ans
{-# INLINE exitF #-}

embedF :: forall ans n m. (ShiftF (n ans) <! m, Monad n) => n ~> m
embedF m = shiftF @(n ans) (m >>=)
{-# INLINE embedF #-}
{-# DEPRECATED shiftF, shiftF', shiftF'', fromShiftF, exitF, embedF "Use Data.Effect.SubJump" #-}
