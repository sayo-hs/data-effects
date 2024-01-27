{-# LANGUAGE AllowAmbiguousTypes #-}

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
module Data.Effect.Reader where

data Ask r (a :: Type) where
    Ask :: Ask r r

data Local r f (a :: Type) where
    Local :: (r -> r) -> f a -> Local r f a

makeEffect [''Ask] [''Local]

asks :: (Ask r <: f, Functor f) => (r -> a) -> f a
asks f = f <$> ask
