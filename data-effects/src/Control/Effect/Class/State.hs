{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Class.State where

class State s f where
    get :: f s
    put :: s -> f ()

makeEffectF ''State

gets :: (State s f, Functor f) => (s -> a) -> f a
gets f = f <$> get

modify :: (State s m, Monad m) => (s -> s) -> m ()
modify f = put . f =<< get
