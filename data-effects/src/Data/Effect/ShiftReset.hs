{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.ShiftReset where

import Control.Monad (void, (>=>))
import Data.Effect.Key.TH qualified as Keyed
import Data.Effect.TH.Internal (noDeriveHFunctor)

data Shift' (r :: Type) m a where
    Shift :: forall r m a. ((a -> m r) -> m r) -> Shift' r m a

makeEffect'
    (def & noDeriveHFunctor & Keyed.changeNormalSenderFnNameFormat)
    Keyed.genEffectKey
    []
    [''Shift']

callCC :: forall r m a. (SendHOEBy ShiftKey (Shift' r) m, Monad m) => ((a -> m r) -> m a) -> m a
callCC f = shift \k -> f (k >=> exit) >>= k

exit :: (SendHOEBy ShiftKey (Shift' r) f, Applicative f) => r -> f a
exit r = shift \_ -> pure r
{-# INLINE exit #-}

getCC :: (SendHOEBy ShiftKey (Shift' r) m, Monad m) => m (m r)
getCC = callCC \exit' -> let a = exit' a in pure a

data Shift_ m a where
    Shift_ :: (forall (r :: Type). (a -> m r) -> m r) -> Shift_ m a

makeEffectH_ [''Shift_]

getCC_ :: (Shift_ <<: m, Monad m) => m (m ())
getCC_ = shift_ \k -> let k' = k $ void k' in k'

data Reset m (a :: Type) where
    Reset :: m a -> Reset m a

makeEffectH [''Reset]
