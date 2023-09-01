-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Reader where

class Reader r f where
    ask :: f r
    local :: (r -> r) -> f a -> f a

makeEffect ''Reader
