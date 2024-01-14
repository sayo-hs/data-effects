{-# LANGUAGE PackageImports #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Prelude (
    module Prelude,
    module Control.Effect.Class.Machinery.TH,
    module Control.Effect.Class.Machinery.TH.Send,
    module Control.Effect.Class.Machinery.TH.DepParams,
    Type,
) where

import Control.Effect.Class.Machinery.TH (
    makeEffect,
    makeEffectF,
    makeEffectH,
    makeEmptyEffect,
 )
import Control.Effect.Class.Machinery.TH.DepParams (
    makeEffectInfoTypeInstances,
 )
import Control.Effect.Class.Machinery.TH.Send (
    EffectOrder (FirstOrder, HigherOrder),
    makeEffectSend,
 )
import Data.Kind (Type)
import "base" Prelude
