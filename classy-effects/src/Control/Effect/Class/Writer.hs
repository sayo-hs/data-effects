{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Writer where

class (Tell w f, WriterH w f) => Writer w f

class Monoid w => Tell w f where
    tell :: w -> f ()

class Monoid w => WriterH w f where
    listen :: f a -> f (a, w)
    cencor :: (w -> w) -> f a -> f a

makeEffectF ''Tell
makeEffectH ''WriterH
makeEffect ''Writer
