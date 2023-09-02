{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Except where

class Throw e (f :: Type -> Type) where
    throw :: e -> f a

class Catch e f where
    catch :: f a -> (e -> f a) -> f a

makeEffect "Except" ''Throw ''Catch
