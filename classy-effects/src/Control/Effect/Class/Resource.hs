-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- The code and documentation before modification is BSD3 licensed,
-- (c) 2019-2023 The Polysemy Lounge: [Polysemy.Resource]
-- (https://hackage.haskell.org/package/polysemy-1.9.1.2/docs/Polysemy-Resource.html).

{- |
Copyright   :  (c) 2019-2023 The Polysemy Lounge
               (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

An effect capable of providing [bracket]
(https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:bracket) semantics.
-}
module Control.Effect.Class.Resource where

import Data.Functor (void)

{- |
An effect capable of providing [bracket]
(https://hackage.haskell.org/package/base-4.16.4.0/docs/Control-Exception.html#v:bracket) semantics.
-}
class Resource f where
    -- | Allocate a resource, use it, and clean it up afterwards.
    bracket :: f a -> (a -> f ()) -> (a -> f b) -> f b

    -- | Allocate a resource, use it, and clean it up afterwards if an error occurred.
    bracketOnExcept :: f a -> (a -> f ()) -> (a -> f b) -> f b

makeEffectH ''Resource

bracket_ :: (Resource f, Functor f) => f a -> f b -> f c -> f c
bracket_ acquire release thing =
    bracket acquire (const $ void release) (const thing)

bracketOnExcept_ :: (Resource f, Functor f) => f a -> f b -> f c -> f c
bracketOnExcept_ acquire onExc thing =
    bracketOnExcept acquire (const $ void onExc) (const thing)

finally :: (Resource f, Applicative f) => f a -> f () -> f a
finally thing release = bracket (pure ()) (const release) (const thing)

finally_ :: (Resource f, Applicative f) => f a -> f b -> f a
finally_ thing release = finally thing (void release)

onException :: (Resource f, Applicative f) => f a -> f () -> f a
onException thing onExc = bracketOnExcept (pure ()) (const onExc) (const thing)

onException_ :: (Resource f, Applicative f) => f a -> f b -> f a
onException_ thing onExc = onException thing (void onExc)
