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
module Data.Effect.Writer where

data Tell w a where
    Tell :: w -> Tell w ()

data WriterH w f a where
    Listen :: f a -> WriterH w f (w, a)
    Censor :: (w -> w) -> f a -> WriterH w f a

makeEffect [''Tell] [''WriterH]

pass :: (Tell w <: m, WriterH w <<: m, Monad m) => m (w -> w, a) -> m a
pass m = do
    (w, (f, a)) <- listen m
    tell $ f w
    pure a
