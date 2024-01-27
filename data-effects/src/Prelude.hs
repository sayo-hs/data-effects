{-# LANGUAGE PackageImports #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Prelude (
    module Prelude,
    module Data.Effect.TH,
    module Control.Effect,
    Type,
) where

import Control.Effect (type (<:), type (<<:))
import Data.Effect.TH (
    makeEffect,
    makeEffectF,
    makeEffectH,
 )
import Data.Kind (Type)
import "base" Prelude
