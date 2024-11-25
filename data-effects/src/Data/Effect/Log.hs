{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.Log where

data Log msg :: Effect where
    Log :: msg -> Log msg f ()
makeEffectF ''Log
