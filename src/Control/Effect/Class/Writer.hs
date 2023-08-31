-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Writer where

class Monoid w => Writer w f where
    tell :: w -> f ()
    listen :: f a -> f (a, w)
    pass :: f (a, w -> w) -> f a

makeEffect ''Writer
