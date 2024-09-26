{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.State where

data State s a where
    Get :: State s s
    Put :: s -> State s ()

makeEffectF [''State]

gets :: (State s <: f, Functor f) => (s -> a) -> f a
gets f = f <$> get

modify :: (State s <: m, Monad m) => (s -> s) -> m ()
modify f = put . f =<< get
