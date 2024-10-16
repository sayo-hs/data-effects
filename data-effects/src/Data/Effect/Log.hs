{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.Log where

data Log msg a where
    Log :: msg -> Log msg ()
makeEffectF [''Log]
