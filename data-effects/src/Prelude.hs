{-# LANGUAGE PackageImports #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Prelude (
    module Prelude,
    module Control.Effect,
    module Control.Effect.Key,
    module Data.Effect.TH,
    module Data.Effect.Key.TH,
    Type,
    def,
    (&),
) where

import Control.Effect (type (<:), type (<<:), type (~>))
import Control.Effect.Key (SendInsBy, SendSigBy)
import Data.Default (Default (def))
import Data.Effect.Key.TH (makeKeyedEffect)
import Data.Effect.TH (makeEffect, makeEffect', makeEffectF, makeEffectH)
import Data.Function ((&))
import Data.Kind (Type)

import "base" Prelude
