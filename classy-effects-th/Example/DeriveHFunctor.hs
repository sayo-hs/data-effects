{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module DeriveHFunctor where

import Control.Effect.Class.TH (makeEffect)

class Except e f where
    throw :: e -> f a
    catch :: f a -> (e -> f a) -> f a

makeEffect ''Except

unit_exception :: IO ()
unit_exception = undefined
