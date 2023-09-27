{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Class.Chronicle where

import Data.Functor (($>))
import Data.These (These (That, These, This))

class ChronicleF c f where
    dictate :: c -> f ()
    confess :: c -> f a

class ChronicleH c f where
    memento :: f a -> f (Either c a)
    absolve :: a -> f a -> f a
    condemn :: f a -> f a

makeEffect "Chronicle" ''ChronicleF ''ChronicleH

chronicle :: (ChronicleF c f, Applicative f) => These c a -> f a
chronicle = \case
    This c -> confess c
    That x -> pure x
    These c x -> dictate c $> x
