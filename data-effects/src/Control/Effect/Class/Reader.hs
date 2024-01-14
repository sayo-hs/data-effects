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
module Control.Effect.Class.Reader where

class Ask (r :: Type) f where
    ask :: f r

class Local r f where
    local :: (r -> r) -> f a -> f a

makeEffect "Reader" ''Ask ''Local

asks :: (Ask r f, Functor f) => (r -> a) -> f a
asks f = f <$> ask
