{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   : (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
              (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides the t`Coroutine` effect, comes
from [@Control.Monad.Freer.Coroutine@](https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Control-Monad-Freer-Coroutine.html)
in the @freer-simple@ package.
-}
module Data.Effect.Coroutine where

import Control.Monad ((>=>))

data Yield a b c where
    Yield :: a -> (b -> c) -> Yield a b c

makeEffectF [''Yield]

data YieldH a b f (c :: Type) where
    YieldH :: a -> (b -> f c) -> YieldH a b f c

makeEffectH [''YieldH]


data Status f a b r =
        Done r
    |   Coroutine a (b -> f (Status f a b r))
    deriving Functor

continueStatus :: Monad f => (x -> f (Status f a b r)) -> Status f a b x -> f (Status f a b r)
continueStatus kk = \case
    Done x -> kk x
    Coroutine a k -> pure . Coroutine a $ k >=> continueStatus kk


replyCoroutine ::
    Applicative f =>
        Yield a b c
    ->  (c -> f (Status f a b r))
    ->  Status f a b r
replyCoroutine (Yield a k) kk = Coroutine a (kk . k)
{-# INLINE replyCoroutine #-}
