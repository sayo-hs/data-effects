{-# LANGUAGE PackageImports #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Prelude (
    module Prelude,
    module Control.Effect,
    module Control.Effect.Key,
    module Data.Effect.TH,
    module Data.Effect.HFunctor.TH,
    module Data.Effect.Key.TH,
    Type,
    Infinite ((:<)),
) where

import Control.Effect (type (<:), type (<<:), type (~>))
import Control.Effect.Key (SendFOEBy, SendHOEBy)
import Data.Effect.HFunctor.TH
import Data.Effect.Key.TH (makeKeyedEffect, makeKeyedEffect_)
import Data.Effect.TH
import Data.Kind (Type)
import Data.List.Infinite (Infinite ((:<)))

import "base" Prelude
