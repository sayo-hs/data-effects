{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.ShiftReset where

import Control.Monad ((>=>))

data Shift (ans :: Type) b :: Effect where
    Shift
        :: forall ans b m a
         . ((a -> b ans) -> (forall x. m x -> b x) -> b ans)
        -> Shift ans b m a
makeEffectH ''Shift

callCC
    :: forall a ans b es ff c
     . (Shift ans b :> es, Monad (Eff ff es), Monad b, Free c ff)
    => ((a -> b ans) -> Eff ff es a)
    -> Eff ff es a
callCC f = shift \k run -> run (f $ k >=> run . abort) >>= k
{-# INLINE callCC #-}

abort
    :: forall a ans b es ff c
     . (Shift ans b :> es, Applicative b, Free c ff)
    => ans
    -> Eff ff es a
abort ans = shift \_ _ -> pure ans
{-# INLINE abort #-}

getCC
    :: forall ans b es ff c
     . (Shift ans b :> es, Monad (Eff ff es), Monad b, Free c ff)
    => Eff ff es (b ans)
getCC = callCC \exit' -> let a = exit' a in pure a

embed :: forall ans b es ff c. (Shift ans b :> es, Monad b, Free c ff) => b ~> Eff ff es
embed m = shift \k _ -> m >>= k
{-# INLINE embed #-}

data Reset m (a :: Type) where
    Reset :: m a -> Reset m a
makeEffectH ''Reset
