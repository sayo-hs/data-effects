{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Example where

import Data.Default (def)
import Data.Effect.TH (makeEffect, makeEffect')
import Data.Effect.TH.Internal (noDeriveHFunctor)
import Data.Function ((&))
import Data.Kind (Type)

data Throw e (a :: Type) where
    Throw :: e -> Throw e a

data Catch e f (a :: Type) where
    Catch :: f a -> (e -> f a) -> Catch e f a

makeEffect [''Throw] [''Catch]

data Unlift b f (a :: Type) where
    WithRunInBase :: ((forall x. f x -> b x) -> b a) -> Unlift b f a

makeEffect' [] [''Unlift] (def & noDeriveHFunctor)
