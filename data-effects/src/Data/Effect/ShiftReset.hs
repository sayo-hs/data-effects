{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.ShiftReset where

import Data.Effect.TH (noExtTemplate)
import Data.Effect.TH.Internal (noDeriveHFunctor)
import Control.Monad ((>=>), void)

data Shift (r :: Type) m a where
    Shift :: forall r m a. ((a -> m r) -> m r) -> Shift r m a

makeEffect' (def & noDeriveHFunctor) noExtTemplate [] [''Shift]

callCC :: forall r m a. (Shift r <<: m, Monad m) => ((a -> m r) -> m a) -> m a
callCC f = shift @r \k -> f (k >=> exit) >>= k

exit :: (Shift r <<: f, Applicative f) => r -> f a
exit r = shift \_ -> pure r
{-# INLINE exit #-}

getCC :: (Shift r <<: m, Monad m) => m (m r)
getCC = callCC \exit' -> let a = exit' a in pure a


data Shift_ m a where
    Shift_ :: (forall (r :: Type). (a -> m r) -> m r) -> Shift_ m a

makeEffect' (def & noDeriveHFunctor) noExtTemplate [] [''Shift_]

getCC_ :: (Shift_ <<: m, Monad m) => m (m ())
getCC_ = shift_ \k -> let k' = k $ void k' in k'


data Reset m (a :: Type) where
    Reset :: m a -> Reset m a

makeEffectH [''Reset]
