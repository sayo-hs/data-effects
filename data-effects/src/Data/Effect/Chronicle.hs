{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Effect.Chronicle where

import Data.Functor (($>))
import Data.These (These (That, These, This))

data ChronicleF c a where
    Dictate :: c -> ChronicleF c ()
    Confess :: c -> ChronicleF c a

data ChronicleH c f a where
    Memento :: f a -> ChronicleH c f (Either c a)
    Absolve :: a -> f a -> ChronicleH c f a
    Condemn :: f a -> ChronicleH c f a

makeEffect [''ChronicleF] [''ChronicleH]

chronicle :: (ChronicleF c <: f, Applicative f) => These c a -> f a
chronicle = \case
    This c -> confess c
    That x -> pure x
    These c x -> dictate c $> x
