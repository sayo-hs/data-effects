{-# LANGUAGE PackageImports #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Prelude (
    module Prelude,
    module Control.Effect,
    module Data.Effect.OpenUnion,
    module Data.Effect,
    module Data.Effect.TH,
    module Data.Effect.HFunctor.TH,
    Type,
    Infinite ((:<)),
) where

import Control.Effect (Eff, Free, type (~>))
import Data.Effect (Effect)
import Data.Effect.HFunctor.TH
import Data.Effect.OpenUnion (Has, In, type (:>))
import Data.Effect.TH
import Data.Kind (Type)
import Data.List.Infinite (Infinite ((:<)))

import "base" Prelude
